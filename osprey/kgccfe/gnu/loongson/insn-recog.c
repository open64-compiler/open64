/* Generated automatically by the program `genrecog' from the target
   machine description file.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "function.h"
#include "insn-config.h"
#include "recog.h"
#include "real.h"
#include "output.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "resource.h"
#include "toplev.h"
#include "reload.h"



/* `recog' contains a decision tree that recognizes whether the rtx
   X0 is a valid instruction.

   recog returns -1 if the rtx is not valid.  If the rtx is valid, recog
   returns a nonnegative number which is the insn code number for the
   pattern that matched.  This is the same as the order in the machine
   description of the entry that matched.  This number can be used as an
   index into `insn_data' and other tables.

   The third argument to recog is an optional pointer to an int.  If
   present, recog will accept a pattern if it matches except for missing
   CLOBBER expressions at the end.  In that case, the value pointed to by
   the optional pointer will be set to the number of CLOBBERs that need
   to be added (it should be initialized to zero by the caller).  If it
   is set nonzero, the caller should allocate a PARALLEL of the
   appropriate size, copy the initial entries, and call add_clobbers
   (found in insn-emit.c) to fill in the CLOBBERs.


   The function split_insns returns 0 if the rtl could not
   be split or the split rtl as an INSN list if it can be.

   The function peephole2_insns returns 0 if the rtl could not
   be matched. If there was a match, the new rtl is returned in an INSN list,
   and LAST_INSN will point to the last recognized insn in the old sequence.
*/


extern rtx gen_split_356 PARAMS ((rtx *));
extern rtx gen_split_357 PARAMS ((rtx *));
extern rtx gen_split_359 PARAMS ((rtx *));
extern rtx gen_split_360 PARAMS ((rtx *));
extern rtx gen_split_361 PARAMS ((rtx *));
extern rtx gen_split_362 PARAMS ((rtx *));
extern rtx gen_split_363 PARAMS ((rtx *));
extern rtx gen_split_364 PARAMS ((rtx *));
extern rtx gen_split_366 PARAMS ((rtx *));
extern rtx gen_split_367 PARAMS ((rtx *));
extern rtx gen_split_369 PARAMS ((rtx *));
extern rtx gen_split_370 PARAMS ((rtx *));
extern rtx gen_split_371 PARAMS ((rtx *));
extern rtx gen_split_372 PARAMS ((rtx *));
extern rtx gen_split_373 PARAMS ((rtx *));
extern rtx gen_split_374 PARAMS ((rtx *));
extern rtx gen_split_378 PARAMS ((rtx *));
extern rtx gen_split_379 PARAMS ((rtx *));
extern rtx gen_split_380 PARAMS ((rtx *));
extern rtx gen_split_381 PARAMS ((rtx *));
extern rtx gen_split_382 PARAMS ((rtx *));
extern rtx gen_split_402 PARAMS ((rtx *));
extern rtx gen_split_405 PARAMS ((rtx *));
extern rtx gen_split_408 PARAMS ((rtx *));
extern rtx gen_split_411 PARAMS ((rtx *));
extern rtx gen_split_412 PARAMS ((rtx *));
extern rtx gen_split_435 PARAMS ((rtx *));
extern rtx gen_split_436 PARAMS ((rtx *));
extern rtx gen_split_439 PARAMS ((rtx *));
extern rtx gen_split_441 PARAMS ((rtx *));
extern rtx gen_split_442 PARAMS ((rtx *));
extern rtx gen_split_443 PARAMS ((rtx *));
extern rtx gen_split_449 PARAMS ((rtx *));
extern rtx gen_split_451 PARAMS ((rtx *));
extern rtx gen_split_454 PARAMS ((rtx *));
extern rtx gen_split_456 PARAMS ((rtx *));
extern rtx gen_split_458 PARAMS ((rtx *));
extern rtx gen_split_460 PARAMS ((rtx *));
extern rtx gen_split_461 PARAMS ((rtx *));
extern rtx gen_split_462 PARAMS ((rtx *));
extern rtx gen_split_463 PARAMS ((rtx *));
extern rtx gen_split_464 PARAMS ((rtx *));
extern rtx gen_split_466 PARAMS ((rtx *));
extern rtx gen_split_468 PARAMS ((rtx *));
extern rtx gen_split_469 PARAMS ((rtx *));
extern rtx gen_split_470 PARAMS ((rtx *));
extern rtx gen_split_471 PARAMS ((rtx *));
extern rtx gen_split_472 PARAMS ((rtx *));
extern rtx gen_split_474 PARAMS ((rtx *));
extern rtx gen_split_475 PARAMS ((rtx *));
extern rtx gen_split_477 PARAMS ((rtx *));
extern rtx gen_split_478 PARAMS ((rtx *));
extern rtx gen_split_479 PARAMS ((rtx *));
extern rtx gen_split_480 PARAMS ((rtx *));
extern rtx gen_split_481 PARAMS ((rtx *));
extern rtx gen_split_507 PARAMS ((rtx *));
extern rtx gen_split_508 PARAMS ((rtx *));
extern rtx gen_split_510 PARAMS ((rtx *));
extern rtx gen_split_511 PARAMS ((rtx *));
extern rtx gen_split_514 PARAMS ((rtx *));
extern rtx gen_split_515 PARAMS ((rtx *));
extern rtx gen_split_518 PARAMS ((rtx *));
extern rtx gen_split_519 PARAMS ((rtx *));
extern rtx gen_split_522 PARAMS ((rtx *));
extern rtx gen_split_523 PARAMS ((rtx *));
extern rtx gen_split_526 PARAMS ((rtx *));
extern rtx gen_split_527 PARAMS ((rtx *));
extern rtx gen_split_542 PARAMS ((rtx *));



static int recog_1 PARAMS ((rtx, rtx, int *));
static int
recog_1 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 0);
  if (GET_CODE (x1) == MEM)
    goto L1584;
  if (register_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L10;
    }
 L3449: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L1626;
    }
  goto ret0;

 L1584: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3454;
    case DImode:
      goto L3455;
    default:
      break;
    }
  goto L3449;

 L3454: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1585;
  goto L3449;

 L1585: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1586;
    }
  goto L3449;

 L1586: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1587;
    }
  goto L3449;

 L1587: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L1588;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1588: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 176;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L3455: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1592;
  goto L3449;

 L1592: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1593;
    }
  goto L3449;

 L1593: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1594;
    }
  goto L3449;

 L1594: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L1595;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1595: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 177;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L10: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DFmode)
    goto L3456;
  x1 = XEXP (x0, 0);
  goto L3449;

 L3456: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L11;
    case MINUS:
      goto L103;
    case MULT:
      goto L195;
    case NEG:
      goto L728;
    case DIV:
      goto L762;
    case SQRT:
      goto L1042;
    case ABS:
      goto L1076;
    case FLOAT_EXTEND:
      goto L1399;
    case FLOAT:
      goto L1450;
    case MEM:
      goto L1557;
    case IF_THEN_ELSE:
      goto L2816;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L3449;

 L11: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L3468;
  x1 = XEXP (x0, 0);
  goto L3449;

 L3468: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L697;
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L12;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L697: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L698;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L698: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L699;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L699: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[3] = x2;
      goto L700;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L700: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD))
    {
      return 55;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L12: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L13;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L13: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 2;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L103: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L3470;
  x1 = XEXP (x0, 0);
  goto L3449;

 L3470: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L713;
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L104;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L713: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L714;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L714: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L715;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L715: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[3] = x2;
      goto L716;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L716: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD))
    {
      return 57;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L104: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L3472;
  x1 = XEXP (x0, 0);
  goto L3449;

 L3472: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L748;
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L105;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L748: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L749;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L749: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      operands[3] = x3;
      goto L750;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L750: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD))
    {
      return 61;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L105: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 16;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L195: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L196;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L196: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L197;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L197: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !TARGET_MIPS4300))
    {
      return 30;
    }
 L203: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_MIPS4300))
    {
      return 31;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L728: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L3473;
  x1 = XEXP (x0, 0);
  goto L3449;

 L3473: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L729;
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1134;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L729: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L730;
  x1 = XEXP (x0, 0);
  goto L3449;

 L730: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L731;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L731: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L732;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L732: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      operands[3] = x3;
      goto L733;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L733: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD))
    {
      return 59;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1134: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 94;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L762: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L3476;
  x1 = XEXP (x0, 0);
  goto L3449;

 L3476: ATTRIBUTE_UNUSED_LABEL
  if (const_float_1_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L775;
    }
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L763;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L775: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L3478;
  x1 = XEXP (x0, 0);
  goto L3449;

 L3478: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == SQRT)
    goto L1054;
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L776;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1054: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L1055;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1055: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_unsafe_math_optimizations))
    {
      return 83;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L776: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_unsafe_math_optimizations))
    {
      return 65;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L763: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L764;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L764: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 63;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1042: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1043;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1043: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && HAVE_SQRT_P() && TARGET_DOUBLE_FLOAT))
    {
      return 81;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1076: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1077;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1077: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 87;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1399: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L1400;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1400: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 142;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1450: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3479;
    case DImode:
      goto L3480;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L3479: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1451;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1451: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 149;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L3480: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1456;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1456: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT))
    {
      return 150;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1557: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3481;
    case DImode:
      goto L3482;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L3481: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1558;
  x1 = XEXP (x0, 0);
  goto L3449;

 L1558: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1559;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1559: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1560;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1560: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 172;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L3482: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1565;
  x1 = XEXP (x0, 0);
  goto L3449;

 L1565: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1566;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1566: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1567;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1567: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 173;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L2816: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2817;
    }
 L2834: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2835;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L2817: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L3483;
    case DImode:
      goto L3484;
    default:
      break;
    }
  goto L2834;

 L3483: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L2818;
    }
  goto L2834;

 L2818: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2819;
  goto L2834;

 L2819: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2820;
    }
  x2 = XEXP (x1, 0);
  goto L2834;

 L2820: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, DFmode))
    {
      operands[3] = x2;
      goto L2821;
    }
  x2 = XEXP (x1, 0);
  goto L2834;

 L2821: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 340;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2834;

 L3484: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2827;
    }
  goto L2834;

 L2827: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2828;
  goto L2834;

 L2828: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2829;
    }
  x2 = XEXP (x1, 0);
  goto L2834;

 L2829: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, DFmode))
    {
      operands[3] = x2;
      goto L2830;
    }
  x2 = XEXP (x1, 0);
  goto L2834;

 L2830: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 341;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2834;

 L2835: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      operands[4] = x3;
      goto L2836;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L2836: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2837;
  x1 = XEXP (x0, 0);
  goto L3449;

 L2837: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2838;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L2838: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2839;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L2839: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 342;
    }
  x1 = XEXP (x0, 0);
  goto L3449;

 L1626: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, DFmode))
    {
      operands[1] = x1;
      goto L1627;
    }
 L1638: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, DFmode))
    {
      operands[1] = x1;
      goto L1639;
    }
  goto ret0;

 L1627: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && !(TARGET_FLOAT64 && !TARGET_64BIT)
   && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || nonmemory_operand (operands[1], DFmode))))
    {
      return 185;
    }
 L1631: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && (TARGET_FLOAT64 && !TARGET_64BIT)
   && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || nonmemory_operand (operands[1], DFmode))))
    {
      return 186;
    }
 L1635: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT) && !TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || nonmemory_operand (operands[1], DFmode))))
    {
      return 187;
    }
  x1 = XEXP (x0, 1);
  goto L1638;

 L1639: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))))
    {
      return 188;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_2 PARAMS ((rtx, rtx, int *));
static int
recog_2 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 0);
  if (GET_CODE (x1) == MEM)
    goto L1570;
  if (register_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L16;
    }
 L3448: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L1614;
    }
  goto ret0;

 L1570: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3485;
    case DImode:
      goto L3486;
    default:
      break;
    }
  goto L3448;

 L3485: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1571;
  goto L3448;

 L1571: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1572;
    }
  goto L3448;

 L1572: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1573;
    }
  goto L3448;

 L1573: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L1574;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1574: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT))
    {
      return 174;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L3486: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1578;
  goto L3448;

 L1578: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1579;
    }
  goto L3448;

 L1579: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1580;
    }
  goto L3448;

 L1580: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L1581;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1581: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT))
    {
      return 175;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L16: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SFmode)
    goto L3487;
  x1 = XEXP (x0, 0);
  goto L3448;

 L3487: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L17;
    case MINUS:
      goto L109;
    case MULT:
      goto L207;
    case NEG:
      goto L737;
    case DIV:
      goto L768;
    case SQRT:
      goto L1047;
    case ABS:
      goto L1081;
    case FLOAT_TRUNCATE:
      goto L1257;
    case FLOAT:
      goto L1460;
    case MEM:
      goto L1543;
    case IF_THEN_ELSE:
      goto L2789;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L3448;

 L17: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L3499;
  x1 = XEXP (x0, 0);
  goto L3448;

 L3499: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L705;
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L18;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L705: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L706;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L706: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L707;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L707: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[3] = x2;
      goto L708;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L708: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_FUSED_MADD))
    {
      return 56;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L18: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L19;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L19: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 3;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L109: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L3501;
  x1 = XEXP (x0, 0);
  goto L3448;

 L3501: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L721;
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L110;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L721: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L722;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L722: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L723;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L723: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[3] = x2;
      goto L724;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L724: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_FUSED_MADD))
    {
      return 58;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L110: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L3503;
  x1 = XEXP (x0, 0);
  goto L3448;

 L3503: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L756;
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L111;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L756: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L757;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L757: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      operands[3] = x3;
      goto L758;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L758: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_FUSED_MADD))
    {
      return 62;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L111: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 17;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L207: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L208;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L208: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L209;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L209: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && !TARGET_MIPS4300))
    {
      return 32;
    }
 L215: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_MIPS4300))
    {
      return 33;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L737: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L3504;
  x1 = XEXP (x0, 0);
  goto L3448;

 L3504: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L738;
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L1139;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L738: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == MULT)
    goto L739;
  x1 = XEXP (x0, 0);
  goto L3448;

 L739: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L740;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L740: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SFmode))
    {
      operands[2] = x4;
      goto L741;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L741: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      operands[3] = x3;
      goto L742;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L742: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_FUSED_MADD))
    {
      return 60;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1139: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 95;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L768: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L3507;
  x1 = XEXP (x0, 0);
  goto L3448;

 L3507: ATTRIBUTE_UNUSED_LABEL
  if (const_float_1_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L781;
    }
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L769;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L781: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L3509;
  x1 = XEXP (x0, 0);
  goto L3448;

 L3509: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == SQRT)
    goto L1061;
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L782;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1061: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L1062;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1062: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && flag_unsafe_math_optimizations))
    {
      return 84;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L782: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT && flag_unsafe_math_optimizations))
    {
      return 66;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L769: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L770;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L770: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 64;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1047: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L1048;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1048: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && HAVE_SQRT_P()))
    {
      return 82;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1081: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L1082;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1082: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 88;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1257: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1258;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1258: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 115;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1460: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3510;
    case DImode:
      goto L3511;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L3510: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1461;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1461: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 151;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L3511: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1466;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1466: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT))
    {
      return 152;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1543: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3512;
    case DImode:
      goto L3513;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L3512: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1544;
  x1 = XEXP (x0, 0);
  goto L3448;

 L1544: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1545;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1545: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1546;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1546: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT))
    {
      return 170;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L3513: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1551;
  x1 = XEXP (x0, 0);
  goto L3448;

 L1551: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1552;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1552: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1553;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1553: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_FP4 && TARGET_HARD_FLOAT))
    {
      return 171;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L2789: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2790;
    }
 L2807: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2808;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L2790: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L3514;
    case DImode:
      goto L3515;
    default:
      break;
    }
  goto L2807;

 L3514: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L2791;
    }
  goto L2807;

 L2791: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2792;
  goto L2807;

 L2792: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2793;
    }
  x2 = XEXP (x1, 0);
  goto L2807;

 L2793: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, SFmode))
    {
      operands[3] = x2;
      goto L2794;
    }
  x2 = XEXP (x1, 0);
  goto L2807;

 L2794: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT))
    {
      return 337;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2807;

 L3515: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2800;
    }
  goto L2807;

 L2800: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2801;
  goto L2807;

 L2801: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2802;
    }
  x2 = XEXP (x1, 0);
  goto L2807;

 L2802: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, SFmode))
    {
      operands[3] = x2;
      goto L2803;
    }
  x2 = XEXP (x1, 0);
  goto L2807;

 L2803: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT))
    {
      return 338;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2807;

 L2808: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      operands[4] = x3;
      goto L2809;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L2809: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2810;
  x1 = XEXP (x0, 0);
  goto L3448;

 L2810: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2811;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L2811: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2812;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L2812: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT))
    {
      return 339;
    }
  x1 = XEXP (x0, 0);
  goto L3448;

 L1614: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, SFmode))
    {
      operands[1] = x1;
      goto L1615;
    }
 L1622: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, SFmode))
    {
      operands[1] = x1;
      goto L1623;
    }
  goto ret0;

 L1615: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || nonmemory_operand (operands[1], SFmode))))
    {
      return 182;
    }
 L1619: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_SOFT_FLOAT && !TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || nonmemory_operand (operands[1], SFmode))))
    {
      return 183;
    }
  x1 = XEXP (x0, 1);
  goto L1622;

 L1623: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))))
    {
      return 184;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_3 PARAMS ((rtx, rtx, int *));
static int
recog_3 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case MINUS:
      goto L127;
    case MULT:
      goto L232;
    case PLUS:
      goto L291;
    case NEG:
      goto L338;
    case TRUNCATE:
      goto L510;
    case DIV:
      goto L917;
    case MOD:
      goto L951;
    case UDIV:
      goto L985;
    case UMOD:
      goto L1019;
    case ABS:
      goto L1066;
    case FFS:
      goto L1096;
    case NOT:
      goto L1143;
    case AND:
      goto L1241;
    case IOR:
      goto L1181;
    case XOR:
      goto L1205;
    case ZERO_EXTEND:
      goto L1298;
    case SIGN_EXTEND:
      goto L1382;
    case FIX:
      goto L1404;
    case UNSPEC:
      goto L3556;
    case HIGH:
      goto L1488;
    case LO_SUM:
      goto L1493;
    case ASHIFT:
      goto L1747;
    case ASHIFTRT:
      goto L1798;
    case LSHIFTRT:
      goto L1849;
    case ROTATERT:
      goto L1906;
    case EQ:
      goto L2031;
    case NE:
      goto L2063;
    case GT:
      goto L2085;
    case GE:
      goto L2109;
    case LT:
      goto L2121;
    case LE:
      goto L2145;
    case GTU:
      goto L2181;
    case GEU:
      goto L2205;
    case LTU:
      goto L2217;
    case LEU:
      goto L2241;
    case IF_THEN_ELSE:
      goto L2735;
    default:
     break;
   }
  goto ret0;

 L127: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L3557;
  goto ret0;

 L3557: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L128;
 L3558: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L317;
    }
  goto ret0;

 L128: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, SImode))
    {
      operands[1] = x2;
      goto L129;
    }
  x2 = XEXP (x1, 0);
  goto L3558;

 L129: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)))
    {
      return 20;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3558;

 L317: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MULT)
    goto L318;
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L135;
    }
  goto ret0;

 L318: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L319;
    }
  goto ret0;

 L319: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L320;
    }
  goto ret0;

 L320: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MADD_MSUB)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 4;
      return 38;
    }
 L366: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MSAC && TARGET_64BIT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 4;
      return 40;
    }
  goto ret0;

 L135: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000))))
    {
      return 21;
    }
  goto ret0;

 L232: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L233;
    }
  goto ret0;

 L233: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L234;
    }
  goto ret0;

 L234: ATTRIBUTE_UNUSED_LABEL
  if ((GENERATE_MULT3_SI
   || TARGET_MAD)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 34;
    }
 L251: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS4000 || TARGET_MIPS16)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 35;
    }
 L270: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS4000 && !TARGET_MIPS16)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 36;
    }
  goto ret0;

 L291: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MULT)
    goto L292;
  goto ret0;

 L292: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L293;
    }
  goto ret0;

 L293: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L294;
    }
  goto ret0;

 L294: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L295;
    }
 L646: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x2, operands[0])
      && (TARGET_MAD)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 52;
    }
  goto ret0;

 L295: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_MIPS3900
   || TARGET_MIPS5400
   || TARGET_MIPS5500
   || ISA_HAS_MADD_MSUB)
   && !TARGET_MIPS16)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 4;
      return 37;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  goto L646;

 L338: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L3559;
  goto ret0;

 L3559: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L339;
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 91;
    }
  goto ret0;

 L339: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L340;
    }
  goto ret0;

 L340: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L341;
    }
  goto ret0;

 L341: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MULS && TARGET_64BIT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 39;
    }
  goto ret0;

 L510: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L3563;
  goto ret0;

 L3563: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ASHIFTRT:
      goto L1278;
    case LSHIFTRT:
      goto L1285;
    case ASHIFT:
      goto L1292;
    default:
     break;
   }
 L3561: ATTRIBUTE_UNUSED_LABEL
  if (highpart_shift_operator (x2, DImode))
    {
      operands[5] = x2;
      goto L511;
    }
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1263;
    }
  goto ret0;

 L1278: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1279;
    }
  goto L3561;

 L1279: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (small_int (x3, DImode))
    {
      operands[2] = x3;
      goto L1280;
    }
  goto L3561;

 L1280: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 119;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3561;

 L1285: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1286;
    }
  goto L3561;

 L1286: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (small_int (x3, DImode))
    {
      operands[2] = x3;
      goto L1287;
    }
  goto L3561;

 L1287: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 120;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3561;

 L1292: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1293;
    }
  goto ret0;

 L1293: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (small_int (x3, DImode))
    {
      operands[2] = x3;
      goto L1294;
    }
  goto ret0;

 L1294: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 121;
    }
  goto ret0;

 L511: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L3566;
  goto ret0;

 L3566: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L512;
    case NEG:
      goto L569;
    default:
     break;
   }
  goto ret0;

 L512: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      operands[3] = x4;
      goto L513;
    }
  goto ret0;

 L513: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[1] = x5;
      goto L514;
    }
  goto ret0;

 L514: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      operands[4] = x4;
      goto L515;
    }
  goto ret0;

 L515: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L516;
    }
  goto ret0;

 L516: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT)
    goto L3568;
  goto ret0;

 L3568: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x3, 0) == 32LL)
    goto L3570;
  goto ret0;

 L3570: ATTRIBUTE_UNUSED_LABEL
  if ((GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 47;
    }
 L3571: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MULHI
   && TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 48;
    }
  goto ret0;

 L569: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L570;
  goto ret0;

 L570: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (extend_operator (x5, DImode))
    {
      operands[3] = x5;
      goto L571;
    }
  goto ret0;

 L571: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[1] = x6;
      goto L572;
    }
  goto ret0;

 L572: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (extend_operator (x5, DImode))
    {
      operands[4] = x5;
      goto L573;
    }
  goto ret0;

 L573: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[2] = x6;
      goto L574;
    }
  goto ret0;

 L574: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL
      && (ISA_HAS_MULHI
   && TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 49;
    }
  goto ret0;

 L1263: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 116;
    }
  goto ret0;

 L917: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L918;
    }
  goto ret0;

 L918: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L919;
    }
  goto ret0;

 L919: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 73;
    }
  goto ret0;

 L951: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L952;
    }
  goto ret0;

 L952: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L953;
    }
  goto ret0;

 L953: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 75;
    }
  goto ret0;

 L985: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L986;
    }
  goto ret0;

 L986: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L987;
    }
  goto ret0;

 L987: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 77;
    }
  goto ret0;

 L1019: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1020;
    }
  goto ret0;

 L1020: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1021;
    }
  goto ret0;

 L1021: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 79;
    }
  goto ret0;

 L1066: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1067;
    }
  goto ret0;

 L1067: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 85;
    }
  goto ret0;

 L1096: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1097;
    }
  goto ret0;

 L1097: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 89;
    }
  goto ret0;

 L1143: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 96;
    }
  goto ret0;

 L1241: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == NOT)
    goto L1242;
  if (uns_arith_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1152;
    }
 L1157: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1158;
    }
  goto ret0;

 L1242: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1243;
    }
  goto ret0;

 L1243: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == NOT)
    goto L1244;
  goto ret0;

 L1244: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1245;
    }
  goto ret0;

 L1245: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 113;
    }
  goto ret0;

 L1152: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1153;
    }
  x2 = XEXP (x1, 0);
  goto L1157;

 L1153: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 98;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L1157;

 L1158: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1159;
    }
  goto ret0;

 L1159: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 99;
    }
  goto ret0;

 L1181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (uns_arith_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1182;
    }
 L1187: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1188;
    }
  goto ret0;

 L1182: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1183;
    }
  x2 = XEXP (x1, 0);
  goto L1187;

 L1183: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 103;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L1187;

 L1188: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1189;
    }
  goto ret0;

 L1189: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 104;
    }
  goto ret0;

 L1205: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (uns_arith_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1206;
    }
  goto ret0;

 L1206: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1207;
    }
  goto ret0;

 L1207: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 107;
    }
 L1213: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 108;
    }
  goto ret0;

 L1298: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case HImode:
      goto L3572;
    case QImode:
      goto L3573;
    default:
      break;
    }
  goto ret0;

 L3572: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == TRUNCATE)
    goto L1299;
  if (nonimmediate_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L1322;
    }
 L3575: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L1327;
    }
  goto ret0;

 L1299: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1300;
    }
  goto ret0;

 L1300: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 122;
    }
  goto ret0;

 L1322: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 126;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3575;

 L1327: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 127;
    }
  goto ret0;

 L3573: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == TRUNCATE)
    goto L1305;
  if (nonimmediate_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L1352;
    }
 L3577: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L1357;
    }
  goto ret0;

 L1305: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1306;
    }
  goto ret0;

 L1306: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 123;
    }
  goto ret0;

 L1352: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 132;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3577;

 L1357: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 133;
    }
  goto ret0;

 L1382: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case HImode:
      goto L3578;
    case QImode:
      goto L3579;
    default:
      break;
    }
  goto ret0;

 L3578: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, HImode))
    {
      operands[1] = x2;
      return 138;
    }
  goto ret0;

 L3579: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, QImode))
    {
      operands[1] = x2;
      return 140;
    }
  goto ret0;

 L1404: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3580;
    case SFmode:
      goto L3581;
    default:
      break;
    }
  goto ret0;

 L3580: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1405;
    }
  goto ret0;

 L1405: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && ISA_HAS_TRUNC_W))
    {
      return 143;
    }
 L1418: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !ISA_HAS_TRUNC_W)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 144;
    }
  goto ret0;

 L3581: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L1423;
    }
  goto ret0;

 L1423: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && ISA_HAS_TRUNC_W))
    {
      return 145;
    }
 L1436: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && !ISA_HAS_TRUNC_W)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 146;
    }
  goto ret0;

 L3556: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 0)
    goto L1470;
  goto ret0;

 L1470: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (general_operand (x2, BLKmode))
    {
      operands[1] = x2;
      goto L1471;
    }
  goto ret0;

 L1471: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 153;
    }
  goto ret0;

 L1488: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (immediate_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1489;
    }
  goto ret0;

 L1489: ATTRIBUTE_UNUSED_LABEL
  if ((mips_split_addresses && !TARGET_MIPS16))
    {
      return 157;
    }
  goto ret0;

 L1493: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1494;
    }
  goto ret0;

 L1494: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1495;
    }
  goto ret0;

 L1495: ATTRIBUTE_UNUSED_LABEL
  if ((mips_split_addresses && !TARGET_MIPS16))
    {
      return 158;
    }
  goto ret0;

 L1747: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1748;
    }
  goto ret0;

 L1748: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1749;
    }
  goto ret0;

 L1749: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 195;
    }
 L1755: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 196;
    }
  goto ret0;

 L1798: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1799;
    }
  goto ret0;

 L1799: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1800;
    }
  goto ret0;

 L1800: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 202;
    }
 L1806: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 203;
    }
  goto ret0;

 L1849: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L3582;
  goto ret0;

 L3582: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1850;
    }
 L3583: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1862;
    }
  goto ret0;

 L1850: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1851;
    }
  x2 = XEXP (x1, 0);
  goto L3583;

 L1851: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 209;
    }
 L1857: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 210;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3583;

 L1862: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1863;
    }
  goto ret0;

 L1863: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 211;
    }
  goto ret0;

 L1906: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1907;
    }
  goto ret0;

 L1907: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1908;
    }
  goto ret0;

 L1908: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_ROTR_SI))
    {
      return 217;
    }
  goto ret0;

 L2031: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2032;
    }
  goto ret0;

 L2032: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT)
    goto L3584;
 L2052: ATTRIBUTE_UNUSED_LABEL
  if (uns_arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2053;
    }
  goto ret0;

 L3584: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x2, 0) == 0LL)
    goto L3586;
  goto L2052;

 L3586: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 231;
    }
 L3587: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 232;
    }
  goto L2052;

 L2053: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 235;
    }
  goto ret0;

 L2063: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2064;
    }
  goto ret0;

 L2064: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL
      && (!TARGET_MIPS16))
    {
      return 237;
    }
 L2074: ATTRIBUTE_UNUSED_LABEL
  if (uns_arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2075;
    }
  goto ret0;

 L2075: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 239;
    }
  goto ret0;

 L2085: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2086;
    }
  goto ret0;

 L2086: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2087;
    }
 L2092: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2093;
    }
  goto ret0;

 L2087: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 241;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  goto L2092;

 L2093: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 242;
    }
  goto ret0;

 L2109: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2110;
    }
  goto ret0;

 L2110: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2111;
    }
  goto ret0;

 L2111: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 245;
    }
  goto ret0;

 L2121: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2122;
    }
  goto ret0;

 L2122: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2123;
    }
  goto ret0;

 L2123: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 247;
    }
 L2129: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 248;
    }
  goto ret0;

 L2145: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2170;
    }
  goto ret0;

 L2170: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2171;
    }
  if (small_int (x2, SImode))
    {
      operands[2] = x2;
      goto L2147;
    }
  goto ret0;

 L2171: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 255;
    }
  goto ret0;

 L2147: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 251;
    }
 L2153: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 252;
    }
  goto ret0;

 L2181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2182;
    }
  goto ret0;

 L2182: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2183;
    }
 L2188: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2189;
    }
  goto ret0;

 L2183: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 257;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  goto L2188;

 L2189: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 258;
    }
  goto ret0;

 L2205: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2206;
    }
  goto ret0;

 L2206: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2207;
    }
  goto ret0;

 L2207: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 261;
    }
  goto ret0;

 L2217: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2218;
    }
  goto ret0;

 L2218: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2219;
    }
  goto ret0;

 L2219: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 263;
    }
 L2225: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 264;
    }
  goto ret0;

 L2241: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2266;
    }
  goto ret0;

 L2266: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2267;
    }
  if (small_int (x2, SImode))
    {
      operands[2] = x2;
      goto L2243;
    }
  goto ret0;

 L2267: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 271;
    }
  goto ret0;

 L2243: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 267;
    }
 L2249: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 268;
    }
  goto ret0;

 L2735: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2736;
    }
 L2753: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2754;
    }
  goto ret0;

 L2736: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L3588;
    case DImode:
      goto L3589;
    default:
      break;
    }
  goto L2753;

 L3588: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L2737;
    }
  goto L2753;

 L2737: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2738;
  goto L2753;

 L2738: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2739;
    }
  x2 = XEXP (x1, 0);
  goto L2753;

 L2739: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2740;
    }
  x2 = XEXP (x1, 0);
  goto L2753;

 L2740: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE))
    {
      return 331;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2753;

 L3589: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2746;
    }
  goto L2753;

 L2746: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2747;
  goto L2753;

 L2747: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2748;
    }
  x2 = XEXP (x1, 0);
  goto L2753;

 L2748: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2749;
    }
  x2 = XEXP (x1, 0);
  goto L2753;

 L2749: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE))
    {
      return 332;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2753;

 L2754: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      operands[4] = x3;
      goto L2755;
    }
  goto ret0;

 L2755: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2756;
  goto ret0;

 L2756: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2757;
    }
  goto ret0;

 L2757: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2758;
    }
  goto ret0;

 L2758: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT))
    {
      return 333;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_4 PARAMS ((rtx, rtx, int *));
static int
recog_4 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case MINUS:
      goto L169;
    case SIGN_EXTEND:
      goto L181;
    case MULT:
      goto L381;
    case NEG:
      goto L460;
    case TRUNCATE:
      goto L594;
    case PLUS:
      goto L663;
    case DIV:
      goto L934;
    case MOD:
      goto L968;
    case UDIV:
      goto L1002;
    case UMOD:
      goto L1036;
    case ABS:
      goto L1071;
    case FFS:
      goto L1111;
    case NOT:
      goto L1147;
    case AND:
      goto L1163;
    case IOR:
      goto L1193;
    case XOR:
      goto L1217;
    case ZERO_EXTEND:
      goto L1316;
    case FIX:
      goto L1440;
    case UNSPEC:
      goto L3629;
    case ASHIFT:
      goto L1786;
    case ASHIFTRT:
      goto L1837;
    case LSHIFTRT:
      goto L1894;
    case ROTATERT:
      goto L1912;
    case EQ:
      goto L2041;
    case NE:
      goto L2068;
    case GT:
      goto L2097;
    case GE:
      goto L2115;
    case LT:
      goto L2133;
    case LE:
      goto L2157;
    case GTU:
      goto L2193;
    case GEU:
      goto L2211;
    case LTU:
      goto L2229;
    case LEU:
      goto L2253;
    case IF_THEN_ELSE:
      goto L2762;
    default:
     break;
   }
  goto ret0;

 L169: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L3630;
  goto ret0;

 L3630: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L170;
 L3631: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L176;
    }
 L3632: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L485;
    }
  goto ret0;

 L170: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, DImode))
    {
      operands[1] = x2;
      goto L171;
    }
  x2 = XEXP (x1, 0);
  goto L3631;

 L171: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)))
    {
      return 26;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3631;

 L176: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L177;
    }
  x2 = XEXP (x1, 0);
  goto L3632;

 L177: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000))))
    {
      return 27;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3632;

 L485: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MULT)
    goto L486;
  goto ret0;

 L486: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      operands[4] = x3;
      goto L487;
    }
  goto ret0;

 L487: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L488;
    }
  goto ret0;

 L488: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      operands[5] = x3;
      goto L489;
    }
  goto ret0;

 L489: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L490;
    }
  goto ret0;

 L490: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT
   && ISA_HAS_MSAC
   && GET_CODE (operands[4]) == GET_CODE (operands[5]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 46;
    }
  goto ret0;

 L181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3633;
    case HImode:
      goto L3635;
    case QImode:
      goto L3636;
    default:
      break;
    }
  goto ret0;

 L3633: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MINUS:
      goto L182;
    case SUBREG:
      goto L3637;
    default:
     break;
   }
  goto ret0;

 L182: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (reg_or_0_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L183;
    }
 L189: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L190;
    }
  goto ret0;

 L183: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L184;
    }
  x3 = XEXP (x2, 0);
  goto L189;

 L184: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)))
    {
      return 28;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L189;

 L190: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L191;
    }
  goto ret0;

 L191: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000))))
    {
      return 29;
    }
  goto ret0;

 L3637: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 0)
    goto L1367;
  goto ret0;

 L1367: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (memory_operand (x3, HImode))
    {
      operands[1] = x3;
      goto L1368;
    }
  goto ret0;

 L1368: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 135;
    }
  goto ret0;

 L3635: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L1378;
    }
  goto ret0;

 L1378: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 137;
    }
  goto ret0;

 L3636: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L1395;
    }
  goto ret0;

 L1395: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 141;
    }
  goto ret0;

 L381: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L3638;
  goto ret0;

 L3638: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L382;
    }
 L3639: ATTRIBUTE_UNUSED_LABEL
  if (extend_operator (x2, DImode))
    {
      operands[3] = x2;
      goto L418;
    }
  goto ret0;

 L382: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L383;
    }
  x2 = XEXP (x1, 0);
  goto L3639;

 L383: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS4000 && !TARGET_MIPS16)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 41;
    }
 L402: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && (GENERATE_MULT3_DI || TARGET_MIPS4000 || TARGET_MIPS16))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 42;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3639;

 L418: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L419;
    }
  goto ret0;

 L419: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (extend_operator (x2, DImode))
    {
      operands[4] = x2;
      goto L420;
    }
  goto ret0;

 L420: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L421;
    }
  goto ret0;

 L421: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 43;
    }
 L442: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 44;
    }
  goto ret0;

 L460: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L3640;
  goto ret0;

 L3640: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L461;
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1129;
    }
  goto ret0;

 L461: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      operands[3] = x3;
      goto L462;
    }
  goto ret0;

 L462: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L463;
    }
  goto ret0;

 L463: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      operands[4] = x3;
      goto L464;
    }
  goto ret0;

 L464: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L465;
    }
  goto ret0;

 L465: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT
   && ISA_HAS_MULS
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 45;
    }
  goto ret0;

 L1129: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 93;
    }
  goto ret0;

 L594: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TImode
      && GET_CODE (x2) == LSHIFTRT)
    goto L595;
  goto ret0;

 L595: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode
      && GET_CODE (x3) == MULT)
    goto L596;
  goto ret0;

 L596: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TImode)
    goto L3642;
  goto ret0;

 L3642: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case SIGN_EXTEND:
      goto L597;
    case ZERO_EXTEND:
      goto L623;
    default:
     break;
   }
  goto ret0;

 L597: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L598;
    }
  goto ret0;

 L598: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == SIGN_EXTEND)
    goto L599;
  goto ret0;

 L599: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L600;
    }
  goto ret0;

 L600: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 64LL
      && (TARGET_64BIT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 50;
    }
  goto ret0;

 L623: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L624;
    }
  goto ret0;

 L624: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == ZERO_EXTEND)
    goto L625;
  goto ret0;

 L625: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L626;
    }
  goto ret0;

 L626: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 64LL
      && (TARGET_64BIT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 51;
    }
  goto ret0;

 L663: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MULT)
    goto L664;
  goto ret0;

 L664: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      operands[3] = x3;
      goto L665;
    }
  goto ret0;

 L665: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L666;
    }
  goto ret0;

 L666: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      operands[4] = x3;
      goto L667;
    }
  goto ret0;

 L667: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L668;
    }
  goto ret0;

 L668: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (rtx_equal_p (x2, operands[0])
      && (TARGET_MAD
   && ! TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 53;
    }
 L692: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x2, operands[0])
      && (TARGET_MAD
   && TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 54;
    }
  goto ret0;

 L934: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L935;
    }
  goto ret0;

 L935: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_nonmemory_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L936;
    }
  goto ret0;

 L936: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 74;
    }
  goto ret0;

 L968: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L969;
    }
  goto ret0;

 L969: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_nonmemory_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L970;
    }
  goto ret0;

 L970: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 76;
    }
  goto ret0;

 L1002: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1003;
    }
  goto ret0;

 L1003: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_nonmemory_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1004;
    }
  goto ret0;

 L1004: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 78;
    }
  goto ret0;

 L1036: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1037;
    }
  goto ret0;

 L1037: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_nonmemory_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1038;
    }
  goto ret0;

 L1038: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 80;
    }
  goto ret0;

 L1071: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1072;
    }
  goto ret0;

 L1072: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 86;
    }
  goto ret0;

 L1111: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1112;
    }
  goto ret0;

 L1112: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 90;
    }
  goto ret0;

 L1147: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 97;
    }
  goto ret0;

 L1163: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L3645;
  goto ret0;

 L3645: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L1250;
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1164;
    }
  goto ret0;

 L1250: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1251;
    }
  goto ret0;

 L1251: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == NOT)
    goto L1252;
  goto ret0;

 L1252: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1253;
    }
  goto ret0;

 L1253: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 114;
    }
  goto ret0;

 L1164: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1165;
    }
 L1176: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1177;
    }
  goto ret0;

 L1165: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16))
    {
      return 100;
    }
 L1171: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && TARGET_MIPS16))
    {
      return 101;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  goto L1176;

 L1177: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 102;
    }
  goto ret0;

 L1193: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1194;
    }
  goto ret0;

 L1194: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1195;
    }
  goto ret0;

 L1195: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16))
    {
      return 105;
    }
 L1201: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && TARGET_MIPS16))
    {
      return 106;
    }
  goto ret0;

 L1217: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1218;
    }
  goto ret0;

 L1218: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1219;
    }
 L1230: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1231;
    }
  goto ret0;

 L1219: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16))
    {
      return 109;
    }
 L1225: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && TARGET_MIPS16))
    {
      return 110;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  goto L1230;

 L1231: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 111;
    }
 L1237: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 112;
    }
  goto ret0;

 L1316: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3646;
    case HImode:
      goto L3647;
    case QImode:
      goto L3649;
    default:
      break;
    }
  goto ret0;

 L3646: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1317;
    }
  goto ret0;

 L1317: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 125;
    }
  goto ret0;

 L3647: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L1332;
    }
 L3648: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L1337;
    }
  goto ret0;

 L1332: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 128;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3648;

 L1337: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 129;
    }
  goto ret0;

 L3649: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L1362;
    }
 L3650: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L1373;
    }
  goto ret0;

 L1362: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 134;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3650;

 L1373: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 136;
    }
  goto ret0;

 L1440: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3651;
    case SFmode:
      goto L3652;
    default:
      break;
    }
  goto ret0;

 L3651: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1441;
    }
  goto ret0;

 L1441: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT))
    {
      return 147;
    }
  goto ret0;

 L3652: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L1446;
    }
  goto ret0;

 L1446: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT))
    {
      return 148;
    }
  goto ret0;

 L3629: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 2)
    goto L1480;
  goto ret0;

 L1480: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (general_operand (x2, BLKmode))
    {
      operands[1] = x2;
      return 155;
    }
  goto ret0;

 L1786: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1787;
    }
  goto ret0;

 L1787: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1788;
    }
  goto ret0;

 L1788: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 200;
    }
 L1794: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 201;
    }
  goto ret0;

 L1837: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1838;
    }
  goto ret0;

 L1838: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1839;
    }
  goto ret0;

 L1839: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 207;
    }
 L1845: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 208;
    }
  goto ret0;

 L1894: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1895;
    }
  goto ret0;

 L1895: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1896;
    }
  goto ret0;

 L1896: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 215;
    }
 L1902: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 216;
    }
  goto ret0;

 L1912: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1913;
    }
  goto ret0;

 L1913: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1914;
    }
  goto ret0;

 L1914: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_ROTR_DI))
    {
      return 218;
    }
  goto ret0;

 L2041: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2042;
    }
  goto ret0;

 L2042: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT)
    goto L3653;
 L2058: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2059;
    }
  goto ret0;

 L3653: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x2, 0) == 0LL)
    goto L3655;
  goto L2058;

 L3655: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 233;
    }
 L3656: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 234;
    }
  goto L2058;

 L2059: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 236;
    }
  goto ret0;

 L2068: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2069;
    }
  goto ret0;

 L2069: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL
      && (TARGET_64BIT && !TARGET_MIPS16))
    {
      return 238;
    }
 L2080: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2081;
    }
  goto ret0;

 L2081: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 240;
    }
  goto ret0;

 L2097: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2098;
    }
  goto ret0;

 L2098: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2099;
    }
 L2104: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2105;
    }
  goto ret0;

 L2099: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 243;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  goto L2104;

 L2105: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 244;
    }
  goto ret0;

 L2115: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2116;
    }
  goto ret0;

 L2116: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2117;
    }
  goto ret0;

 L2117: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 246;
    }
  goto ret0;

 L2133: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2134;
    }
  goto ret0;

 L2134: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2135;
    }
  goto ret0;

 L2135: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 249;
    }
 L2141: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 250;
    }
  goto ret0;

 L2157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2176;
    }
  goto ret0;

 L2176: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2177;
    }
  if (small_int (x2, DImode))
    {
      operands[2] = x2;
      goto L2159;
    }
  goto ret0;

 L2177: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 256;
    }
  goto ret0;

 L2159: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 253;
    }
 L2165: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 254;
    }
  goto ret0;

 L2193: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2194;
    }
  goto ret0;

 L2194: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2195;
    }
 L2200: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2201;
    }
  goto ret0;

 L2195: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 259;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  goto L2200;

 L2201: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 260;
    }
  goto ret0;

 L2211: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2212;
    }
  goto ret0;

 L2212: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2213;
    }
  goto ret0;

 L2213: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 262;
    }
  goto ret0;

 L2229: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2230;
    }
  goto ret0;

 L2230: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2231;
    }
  goto ret0;

 L2231: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 265;
    }
 L2237: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16))
    {
      return 266;
    }
  goto ret0;

 L2253: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2272;
    }
  goto ret0;

 L2272: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2273;
    }
  if (small_int (x2, DImode))
    {
      operands[2] = x2;
      goto L2255;
    }
  goto ret0;

 L2273: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16))
    {
      return 272;
    }
  goto ret0;

 L2255: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 269;
    }
 L2261: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767))
    {
      return 270;
    }
  goto ret0;

 L2762: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2763;
    }
 L2780: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2781;
    }
  goto ret0;

 L2763: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L3657;
    case DImode:
      goto L3658;
    default:
      break;
    }
  goto L2780;

 L3657: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L2764;
    }
  goto L2780;

 L2764: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2765;
  goto L2780;

 L2765: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2766;
    }
  x2 = XEXP (x1, 0);
  goto L2780;

 L2766: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2767;
    }
  x2 = XEXP (x1, 0);
  goto L2780;

 L2767: ATTRIBUTE_UNUSED_LABEL
  if (((ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE) && TARGET_64BIT))
    {
      return 334;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2780;

 L3658: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2773;
    }
  goto L2780;

 L2773: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2774;
  goto L2780;

 L2774: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2775;
    }
  x2 = XEXP (x1, 0);
  goto L2780;

 L2775: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2776;
    }
  x2 = XEXP (x1, 0);
  goto L2780;

 L2776: ATTRIBUTE_UNUSED_LABEL
  if (((ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE) && TARGET_64BIT))
    {
      return 335;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2780;

 L2781: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      operands[4] = x3;
      goto L2782;
    }
  goto ret0;

 L2782: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2783;
  goto ret0;

 L2783: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2784;
    }
  goto ret0;

 L2784: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2785;
    }
  goto ret0;

 L2785: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_64BIT))
    {
      return 336;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_5 PARAMS ((rtx, rtx, int *));
static int
recog_5 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 0);
  switch (GET_MODE (x1))
    {
    case DFmode:
      goto L3445;
    case SFmode:
      goto L3444;
    case SImode:
      goto L3441;
    case DImode:
      goto L3450;
    case HImode:
      goto L3436;
    case QImode:
      goto L3437;
    case BLKmode:
      goto L3438;
    case CCmode:
      goto L3443;
    default:
      break;
    }
 L1916: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == PC)
    goto L2394;
  if (register_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L2508;
    }
  goto ret0;

 L3445: ATTRIBUTE_UNUSED_LABEL
  tem = recog_1 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  goto L1916;

 L3444: ATTRIBUTE_UNUSED_LABEL
  tem = recog_2 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  goto L1916;

 L3441: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == MEM)
    goto L1522;
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L22;
    }
 L3427: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == REG
      && XINT (x1, 0) == 29)
    goto L28;
 L3428: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L34;
    }
 L3432: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == REG
      && XINT (x1, 0) == 29)
    goto L120;
 L3433: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L126;
    }
 L3442: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L1528;
    }
 L3452: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2866;
    }
  goto L1916;

 L1522: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L1523;
  goto L3442;

 L1523: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 29)
    goto L1524;
  goto L3442;

 L1524: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (small_int (x3, SImode))
    {
      operands[0] = x3;
      goto L1525;
    }
  goto L3442;

 L1525: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == REG
      && XINT (x1, 0) == 31
      && (TARGET_MIPS16))
    {
      return 165;
    }
  x1 = XEXP (x0, 0);
  goto L3442;

 L22: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == PLUS)
    goto L23;
  x1 = XEXP (x0, 0);
  goto L3427;

 L23: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L24;
    }
  x1 = XEXP (x0, 0);
  goto L3427;

 L24: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L25;
    }
  x1 = XEXP (x0, 0);
  goto L3427;

 L25: ATTRIBUTE_UNUSED_LABEL
  if ((! TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)))
    {
      return 4;
    }
  x1 = XEXP (x0, 0);
  goto L3427;

 L28: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == PLUS)
    goto L29;
  x1 = XEXP (x0, 0);
  goto L3428;

 L29: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L30;
  x1 = XEXP (x0, 0);
  goto L3428;

 L30: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, SImode))
    {
      operands[0] = x2;
      goto L31;
    }
  x1 = XEXP (x0, 0);
  goto L3428;

 L31: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 5;
    }
  x1 = XEXP (x0, 0);
  goto L3428;

 L34: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L3516;
  x1 = XEXP (x0, 0);
  goto L3432;

 L3516: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L35;
    case MINUS:
      goto L115;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L3432;

 L35: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L3518;
  x1 = XEXP (x0, 0);
  goto L3432;

 L3518: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L36;
 L3519: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L42;
    }
  x1 = XEXP (x0, 0);
  goto L3432;

 L36: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, SImode))
    {
      operands[1] = x2;
      goto L37;
    }
  x2 = XEXP (x1, 0);
  goto L3519;

 L37: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 6;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3519;

 L42: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L43;
    }
  x1 = XEXP (x0, 0);
  goto L3432;

 L43: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (GET_CODE (operands[1]) != REG
       || REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[1]))
       || REGNO (operands[1]) == ARG_POINTER_REGNUM
       || REGNO (operands[1]) == FRAME_POINTER_REGNUM
       || REGNO (operands[1]) == STACK_POINTER_REGNUM)
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[2]))
       || REGNO (operands[2]) == ARG_POINTER_REGNUM
       || REGNO (operands[2]) == FRAME_POINTER_REGNUM
       || REGNO (operands[2]) == STACK_POINTER_REGNUM)))
    {
      return 7;
    }
  x1 = XEXP (x0, 0);
  goto L3432;

 L115: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L116;
    }
  x1 = XEXP (x0, 0);
  goto L3432;

 L116: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L117;
    }
  x1 = XEXP (x0, 0);
  goto L3432;

 L117: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)))
    {
      return 18;
    }
  x1 = XEXP (x0, 0);
  goto L3432;

 L120: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == MINUS)
    goto L121;
  x1 = XEXP (x0, 0);
  goto L3433;

 L121: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L122;
  x1 = XEXP (x0, 0);
  goto L3433;

 L122: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, SImode))
    {
      operands[0] = x2;
      goto L123;
    }
  x1 = XEXP (x0, 0);
  goto L3433;

 L123: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)))
    {
      return 19;
    }
  x1 = XEXP (x0, 0);
  goto L3433;

 L126: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L3520;
  x1 = XEXP (x0, 0);
  goto L3442;

 L3520: ATTRIBUTE_UNUSED_LABEL
  tem = recog_3 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L3442;

 L1528: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, SImode))
    {
      operands[1] = x1;
      goto L1529;
    }
  x1 = XEXP (x0, 0);
  goto L3452;

 L1529: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))))
    {
      return 166;
    }
 L1533: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[0]) == MEM
	   && GET_CODE (XEXP (operands[0], 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == CONST
	   && mips16_gp_offset_p (XEXP (XEXP (operands[0], 0), 1))
	   && GET_CODE (operands[1]) == CONST_INT
	   && (SMALL_INT (operands[1])
	       || SMALL_INT_UNSIGNED (operands[1]))))))
    {
      return 167;
    }
  x1 = XEXP (x0, 0);
  goto L3452;

 L2866: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (address_operand (x1, SImode))
    {
      operands[1] = x1;
      goto L2867;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2867: ATTRIBUTE_UNUSED_LABEL
  if ((Pmode == SImode))
    {
      return 352;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3450: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == REG
      && XINT (x1, 0) == 28)
    goto L1650;
 L3429: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L64;
    }
 L3430: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == REG
      && XINT (x1, 0) == 29)
    goto L70;
 L3431: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L76;
    }
 L3434: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == REG
      && XINT (x1, 0) == 29)
    goto L162;
 L3435: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L168;
    }
 L3439: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1498;
    }
 L3440: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1513;
    }
 L3453: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2870;
    }
  goto L1916;

 L1650: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == UNSPEC_VOLATILE
      && XVECLEN (x1, 0) == 2
      && XINT (x1, 1) == 7)
    goto L1651;
  x1 = XEXP (x0, 0);
  goto L3429;

 L1651: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (address_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1652;
    }
  x1 = XEXP (x0, 0);
  goto L3429;

 L1652: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1653;
    }
  x1 = XEXP (x0, 0);
  goto L3429;

 L1653: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 189;
    }
  x1 = XEXP (x0, 0);
  goto L3429;

 L64: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == PLUS)
    goto L65;
  x1 = XEXP (x0, 0);
  goto L3430;

 L65: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L66;
    }
  x1 = XEXP (x0, 0);
  goto L3430;

 L66: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L67;
    }
  x1 = XEXP (x0, 0);
  goto L3430;

 L67: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT
   && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)))
    {
      return 10;
    }
  x1 = XEXP (x0, 0);
  goto L3430;

 L70: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == PLUS)
    goto L71;
  x1 = XEXP (x0, 0);
  goto L3431;

 L71: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L72;
  x1 = XEXP (x0, 0);
  goto L3431;

 L72: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, DImode))
    {
      operands[0] = x2;
      goto L73;
    }
  x1 = XEXP (x0, 0);
  goto L3431;

 L73: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT))
    {
      return 11;
    }
  x1 = XEXP (x0, 0);
  goto L3431;

 L76: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L3590;
  x1 = XEXP (x0, 0);
  goto L3434;

 L3590: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L77;
    case SIGN_EXTEND:
      goto L89;
    case MINUS:
      goto L157;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L3434;

 L77: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L3593;
  x1 = XEXP (x0, 0);
  goto L3434;

 L3593: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L78;
 L3594: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L84;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L78: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, DImode))
    {
      operands[1] = x2;
      goto L79;
    }
  x2 = XEXP (x1, 0);
  goto L3594;

 L79: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT))
    {
      return 12;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3594;

 L84: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L85;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L85: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT
   && (GET_CODE (operands[1]) != REG
       || REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[1]))
       || REGNO (operands[1]) == ARG_POINTER_REGNUM
       || REGNO (operands[1]) == FRAME_POINTER_REGNUM
       || REGNO (operands[1]) == STACK_POINTER_REGNUM)
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[2]))
       || REGNO (operands[2]) == ARG_POINTER_REGNUM
       || REGNO (operands[2]) == FRAME_POINTER_REGNUM
       || REGNO (operands[2]) == STACK_POINTER_REGNUM)))
    {
      return 13;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L89: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L90;
  x1 = XEXP (x0, 0);
  goto L3434;

 L90: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (reg_or_0_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L91;
    }
 L97: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L98;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L91: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L92;
    }
  x3 = XEXP (x2, 0);
  goto L97;

 L92: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT
   && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)))
    {
      return 14;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L97;

 L98: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L99;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L99: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT))
    {
      return 15;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_reg_or_0_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L158;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L158: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L159;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L159: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)))
    {
      return 24;
    }
  x1 = XEXP (x0, 0);
  goto L3434;

 L162: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == MINUS)
    goto L163;
  x1 = XEXP (x0, 0);
  goto L3435;

 L163: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 29)
    goto L164;
  x1 = XEXP (x0, 0);
  goto L3435;

 L164: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (small_int (x2, DImode))
    {
      operands[0] = x2;
      goto L165;
    }
  x1 = XEXP (x0, 0);
  goto L3435;

 L165: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)))
    {
      return 25;
    }
  x1 = XEXP (x0, 0);
  goto L3435;

 L168: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L3595;
  x1 = XEXP (x0, 0);
  goto L3439;

 L3595: ATTRIBUTE_UNUSED_LABEL
  tem = recog_4 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L3439;

 L1498: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == REG
      && XINT (x1, 0) == 31
      && (TARGET_MIPS16 && TARGET_64BIT))
    {
      return 159;
    }
  x1 = XEXP (x0, 0);
  goto L3440;

 L1513: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == SIGN_EXTEND)
    goto L1514;
  if (general_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L1502;
    }
 L1509: ATTRIBUTE_UNUSED_LABEL
  if (move_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L1510;
    }
 L1518: ATTRIBUTE_UNUSED_LABEL
  if (movdi_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L1519;
    }
  x1 = XEXP (x0, 0);
  goto L3453;

 L1514: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (move_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1515;
    }
  goto L1518;

 L1515: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode))))
    {
      return 163;
    }
  x1 = XEXP (x0, 1);
  goto L1518;

 L1502: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode))))
    {
      return 160;
    }
 L1506: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))))
    {
      return 161;
    }
  x1 = XEXP (x0, 1);
  goto L1509;

 L1510: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode))))
    {
      return 162;
    }
  x1 = XEXP (x0, 1);
  goto L1518;

 L1519: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || se_register_operand (operands[1], DImode))))
    {
      return 164;
    }
  x1 = XEXP (x0, 0);
  goto L3453;

 L2870: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (address_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L2871;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2871: ATTRIBUTE_UNUSED_LABEL
  if ((Pmode == DImode))
    {
      return 353;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3436: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L1266;
    }
 L3446: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L1598;
    }
  goto L1916;

 L1266: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == HImode)
    goto L3659;
  x1 = XEXP (x0, 0);
  goto L3446;

 L3659: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case TRUNCATE:
      goto L1267;
    case ZERO_EXTEND:
      goto L1310;
    case SIGN_EXTEND:
      goto L1386;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1267: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1268;
    }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1268: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 117;
    }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1310: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == QImode)
    goto L3662;
  x1 = XEXP (x0, 0);
  goto L3446;

 L3662: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == TRUNCATE)
    goto L1311;
  if (nonimmediate_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L1342;
    }
 L3664: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L1347;
    }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1311: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1312;
    }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1312: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 124;
    }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1342: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 130;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3664;

 L1347: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 131;
    }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1386: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (memory_operand (x2, QImode))
    {
      operands[1] = x2;
      return 139;
    }
  x1 = XEXP (x0, 0);
  goto L3446;

 L1598: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, HImode))
    {
      operands[1] = x1;
      goto L1599;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L1599: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))))
    {
      return 178;
    }
 L1603: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))))
    {
      return 179;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3437: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, QImode))
    {
      operands[0] = x1;
      goto L1271;
    }
 L3447: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, QImode))
    {
      operands[0] = x1;
      goto L1606;
    }
  goto L1916;

 L1271: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == QImode
      && GET_CODE (x1) == TRUNCATE)
    goto L1272;
  x1 = XEXP (x0, 0);
  goto L3447;

 L1272: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1273;
    }
  x1 = XEXP (x0, 0);
  goto L3447;

 L1273: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 118;
    }
  x1 = XEXP (x0, 0);
  goto L3447;

 L1606: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, QImode))
    {
      operands[1] = x1;
      goto L1607;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L1607: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))))
    {
      return 180;
    }
 L1611: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))))
    {
      return 181;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3438: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x1, BLKmode))
    {
      operands[0] = x1;
      goto L1474;
    }
  goto L1916;

 L1474: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BLKmode)
    goto L3665;
  x1 = XEXP (x0, 0);
  goto L1916;

 L3665: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == UNSPEC)
    goto L3667;
  x1 = XEXP (x0, 0);
  goto L1916;

 L3667: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1)
    goto L3669;
  x1 = XEXP (x0, 0);
  goto L1916;

 L3669: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x1, 1))
    {
    case 1LL:
      goto L1475;
    case 3LL:
      goto L1484;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L1475: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1476;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L1476: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 154;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L1484: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (reg_or_0_operand (x2, DImode))
    {
      operands[1] = x2;
      return 156;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3443: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, CCmode))
    {
      operands[0] = x1;
      goto L1538;
    }
 L3451: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, CCmode))
    {
      operands[0] = x1;
      goto L2276;
    }
  goto L1916;

 L1538: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, CCmode))
    {
      operands[1] = x1;
      goto L1539;
    }
  x1 = XEXP (x0, 0);
  goto L3451;

 L1539: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_8CC && TARGET_HARD_FLOAT))
    {
      return 169;
    }
  x1 = XEXP (x0, 0);
  goto L3451;

 L2276: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == CCmode)
    goto L3671;
  x1 = XEXP (x0, 0);
  goto L1916;

 L3671: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case UNORDERED:
      goto L2277;
    case UNLT:
      goto L2283;
    case UNEQ:
      goto L2289;
    case UNLE:
      goto L2295;
    case EQ:
      goto L2301;
    case LT:
      goto L2307;
    case LE:
      goto L2313;
    case GT:
      goto L2319;
    case GE:
      goto L2325;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2277: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3680;
    case SFmode:
      goto L3681;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3680: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2278;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2278: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2279;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2279: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 273;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3681: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2332;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2332: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2333;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2333: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 282;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2283: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3682;
    case SFmode:
      goto L3683;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3682: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2284;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2284: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2285;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2285: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 274;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3683: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2338;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2338: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2339;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2339: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 283;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2289: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3684;
    case SFmode:
      goto L3685;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3684: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2290;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2290: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2291;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2291: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 275;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3685: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2344;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2344: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2345;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2345: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 284;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2295: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3686;
    case SFmode:
      goto L3687;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3686: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2296;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2296: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2297;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2297: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 276;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3687: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2350;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2350: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2351;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2351: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 285;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2301: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3688;
    case SFmode:
      goto L3689;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3688: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2302;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2302: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2303;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2303: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 277;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3689: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2356;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2356: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2357;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2357: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 286;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2307: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3690;
    case SFmode:
      goto L3691;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3690: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2308;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2308: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2309;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2309: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 278;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3691: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2362;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2362: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2363;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2363: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 287;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2313: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3692;
    case SFmode:
      goto L3693;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3692: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2314;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2314: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2315;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2315: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 279;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3693: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2368;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2368: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2369;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2369: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 288;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2319: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3694;
    case SFmode:
      goto L3695;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3694: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2320;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2320: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2321;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2321: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 280;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3695: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2374;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2374: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2375;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2375: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 289;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2325: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L3696;
    case SFmode:
      goto L3697;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3696: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2326;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2326: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L2327;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2327: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT))
    {
      return 281;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L3697: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2380;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2380: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L2381;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2381: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 290;
    }
  x1 = XEXP (x0, 0);
  goto L1916;

 L2394: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_MODE (x1))
    {
    case SImode:
      goto L3698;
    case DImode:
      goto L3699;
    default:
      break;
    }
 L1917: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case IF_THEN_ELSE:
      goto L1918;
    case LABEL_REF:
      goto L2385;
    default:
     break;
   }
  goto ret0;

 L3698: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2395;
    }
  goto L1917;

 L2395: ATTRIBUTE_UNUSED_LABEL
  if ((!(Pmode == DImode)))
    {
      return 293;
    }
  x1 = XEXP (x0, 1);
  goto L1917;

 L3699: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2399;
    }
  goto L1917;

 L2399: ATTRIBUTE_UNUSED_LABEL
  if ((Pmode == DImode))
    {
      return 294;
    }
  x1 = XEXP (x0, 1);
  goto L1917;

 L1918: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case CCmode:
      goto L3700;
    case SImode:
      goto L3701;
    case DImode:
      goto L3702;
    default:
      break;
    }
  goto ret0;

 L3700: ATTRIBUTE_UNUSED_LABEL
  if (cmp_op (x2, CCmode))
    {
      operands[0] = x2;
      goto L1919;
    }
  goto ret0;

 L1919: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      operands[2] = x3;
      goto L1920;
    }
  goto ret0;

 L1920: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1921;
  goto ret0;

 L1921: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1922;
    case PC:
      goto L1931;
    default:
     break;
   }
  goto ret0;

 L1922: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1923;

 L1923: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC
      && (TARGET_HARD_FLOAT))
    {
      return 219;
    }
  goto ret0;

 L1931: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == LABEL_REF)
    goto L1932;
  goto ret0;

 L1932: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1933;

 L1933: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT))
    {
      return 220;
    }
  goto ret0;

 L3701: ATTRIBUTE_UNUSED_LABEL
  if (cmp_op (x2, SImode))
    {
      operands[0] = x2;
      goto L1938;
    }
 L3703: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, SImode))
    {
      operands[0] = x2;
      goto L1976;
    }
  goto ret0;

 L1938: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1939;
    }
  goto L3703;

 L1939: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1940;
  goto L3703;

 L1940: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1941;
    case PC:
      goto L1950;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L3703;

 L1941: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1942;

 L1942: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC
      && (!TARGET_MIPS16))
    {
      return 221;
    }
  x2 = XEXP (x1, 0);
  goto L3703;

 L1950: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == LABEL_REF)
    goto L1951;
  x2 = XEXP (x1, 0);
  goto L3703;

 L1951: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1952;

 L1952: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 222;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3703;

 L1976: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode)
    goto L3705;
  goto ret0;

 L3705: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1977;
    }
 L3706: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L2015;
    }
  goto ret0;

 L1977: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L1978;
    }
  x3 = XEXP (x2, 0);
  goto L3706;

 L1978: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1979;
    case PC:
      goto L1997;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3706;

 L1979: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1980;

 L1980: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC
      && (!TARGET_MIPS16))
    {
      return 225;
    }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3706;

 L1997: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == LABEL_REF)
    goto L1998;
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3706;

 L1998: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1999;

 L1999: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 227;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3706;

 L2015: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2016;
  goto ret0;

 L2016: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      operands[2] = x2;
      goto L2017;
    }
  goto ret0;

 L2017: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2018;
    }
  goto ret0;

 L2018: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 229;
    }
  goto ret0;

 L3702: ATTRIBUTE_UNUSED_LABEL
  if (cmp_op (x2, DImode))
    {
      operands[0] = x2;
      goto L1957;
    }
 L3704: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, DImode))
    {
      operands[0] = x2;
      goto L1985;
    }
  goto ret0;

 L1957: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1958;
    }
  goto L3704;

 L1958: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1959;
  goto L3704;

 L1959: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1960;
    case PC:
      goto L1969;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L3704;

 L1960: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1961;

 L1961: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC
      && (!TARGET_MIPS16))
    {
      return 223;
    }
  x2 = XEXP (x1, 0);
  goto L3704;

 L1969: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == LABEL_REF)
    goto L1970;
  x2 = XEXP (x1, 0);
  goto L3704;

 L1970: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1971;

 L1971: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 224;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3704;

 L1985: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L3707;
  goto ret0;

 L3707: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1986;
    }
 L3708: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2024;
    }
  goto ret0;

 L1986: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L1987;
    }
  x3 = XEXP (x2, 0);
  goto L3708;

 L1987: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1988;
    case PC:
      goto L2007;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3708;

 L1988: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L1989;

 L1989: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC
      && (!TARGET_MIPS16))
    {
      return 226;
    }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3708;

 L2007: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == LABEL_REF)
    goto L2008;
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3708;

 L2008: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L2009;

 L2009: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 228;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  goto L3708;

 L2024: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2025;
  goto ret0;

 L2025: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      operands[2] = x2;
      goto L2026;
    }
  goto ret0;

 L2026: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2027;
    }
  goto ret0;

 L2027: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 230;
    }
  goto ret0;

 L2385: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  operands[0] = x2;
  goto L2386;

 L2386: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 291;
    }
 L2391: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 292;
    }
  goto ret0;

 L2508: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 4)
    goto L2509;
  goto ret0;

 L2509: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  operands[1] = x2;
  goto L2510;

 L2510: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_EMBEDDED_PIC
   && GET_CODE (operands[1]) == SYMBOL_REF)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 304;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_6 PARAMS ((rtx, rtx, int *));
static int
recog_6 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L3712;
    case SImode:
      goto L3711;
    default:
      break;
    }
 L2402: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PC)
    goto L2403;
  if (register_operand (x2, VOIDmode))
    {
      operands[0] = x2;
      goto L2502;
    }
  goto ret0;

 L3712: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == REG
      && XINT (x2, 0) == 28)
    goto L1643;
 L3710: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L47;
    }
  goto L2402;

 L1643: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC_VOLATILE
      && XVECLEN (x2, 0) == 2
      && XINT (x2, 1) == 7)
    goto L1644;
  x2 = XEXP (x1, 0);
  goto L3710;

 L1644: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (address_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L1645;
    }
  x2 = XEXP (x1, 0);
  goto L3710;

 L1645: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1646;
    }
  x2 = XEXP (x1, 0);
  goto L3710;

 L1646: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1647;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L3710;

 L1647: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1)
    {
      return 189;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L3710;

 L47: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L3713;
  x2 = XEXP (x1, 0);
  goto L2402;

 L3713: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L48;
    case MINUS:
      goto L140;
    case MULT:
      goto L407;
    case DIV:
      goto L826;
    case UDIV:
      goto L876;
    case NEG:
      goto L1121;
    case ASHIFT:
      goto L1760;
    case ASHIFTRT:
      goto L1811;
    case LSHIFTRT:
      goto L1868;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L2402;

 L48: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L3723;
  x2 = XEXP (x1, 0);
  goto L2402;

 L3723: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L652;
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L49;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L652: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      operands[3] = x4;
      goto L653;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L653: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[1] = x5;
      goto L654;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L654: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      operands[4] = x4;
      goto L655;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L655: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L656;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L656: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[0]))
    goto L657;
  x2 = XEXP (x1, 0);
  goto L2402;

 L657: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L658;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L658: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L659;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L659: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MAD
   && ! TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 53;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L49: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L50;
    }
  if (small_int (x3, DImode))
    {
      operands[2] = x3;
      goto L59;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L50: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L51;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L51: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L52;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L52: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
    {
      return 8;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L59: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L60;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L60: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L61;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L61: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)))
    {
      return 9;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L140: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L141;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L141: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L142;
    }
  if (small_int (x3, DImode))
    {
      operands[2] = x3;
      goto L151;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L142: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L143;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L143: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L144;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L144: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
    {
      return 22;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L151: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L152;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L152: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L153;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L153: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && INTVAL (operands[2]) != -32768))
    {
      return 23;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L407: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      operands[3] = x3;
      goto L408;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L408: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L409;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L409: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      operands[4] = x3;
      goto L410;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L410: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L411;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L411: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L412;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L412: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L413;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L413: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 43;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L826: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L827;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L827: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L828;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L828: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L829;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L829: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L830;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L830: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MOD)
    goto L831;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L831: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L832;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L832: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2])
      && (TARGET_64BIT && optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 68;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L876: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L877;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L877: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L878;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L878: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L879;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L879: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L880;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L880: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UMOD)
    goto L881;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L881: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L882;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L882: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2])
      && (TARGET_64BIT && optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 70;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1121: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1122;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1122: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1123;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1123: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1124;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1124: ATTRIBUTE_UNUSED_LABEL
  if ((! TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
    {
      return 92;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1760: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1761;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1761: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1762;
    }
  if (small_int (x3, SImode))
    {
      operands[2] = x3;
      goto L1771;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1762: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1763;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1763: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1764;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1764: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
    {
      return 197;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1771: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1772;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1772: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1773;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1773: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return 198;
    }
 L1782: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return 199;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1811: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1812;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1812: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1813;
    }
  if (small_int (x3, SImode))
    {
      operands[2] = x3;
      goto L1822;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1813: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1814;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1814: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1815;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1815: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
    {
      return 204;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1822: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1823;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1823: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1824;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1824: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0))
    {
      return 205;
    }
 L1833: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return 206;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1868: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1869;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1869: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1870;
    }
  if (small_int (x3, SImode))
    {
      operands[2] = x3;
      goto L1879;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1870: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1871;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1871: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1872;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1872: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
    {
      return 212;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1879: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1880;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1880: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1881;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1881: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return 213;
    }
 L1890: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return 214;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L3711: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L800;
    }
  goto L2402;

 L800: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L3724;
  x2 = XEXP (x1, 0);
  goto L2402;

 L3724: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case DIV:
      goto L801;
    case UDIV:
      goto L851;
    case FIX:
      goto L1410;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L2402;

 L801: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L802;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L802: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L803;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L803: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L804;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L804: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L805;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L805: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MOD)
    goto L806;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L806: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L807;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L807: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2])
      && (optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 67;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L851: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L852;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L852: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L853;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L853: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L854;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L854: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L855;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L855: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == UMOD)
    goto L856;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L856: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L857;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L857: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2])
      && (optimize)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 69;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1410: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DFmode:
      goto L3727;
    case SFmode:
      goto L3728;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L3727: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L1411;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1411: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1412;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1412: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L1413;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1413: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !ISA_HAS_TRUNC_W))
    {
      return 144;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L3728: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L1429;
    }
  x2 = XEXP (x1, 0);
  goto L2402;

 L1429: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1430;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1430: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L1431;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L1431: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_HARD_FLOAT && !ISA_HAS_TRUNC_W))
    {
      return 146;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2402;

 L2403: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3731;
    case DImode:
      goto L3732;
    default:
      break;
    }
  goto ret0;

 L3731: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2420;
    case MEM:
      goto L2454;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L3729;
    default:
      goto ret0;
   }
 L3729: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L2404;
    }
  goto ret0;

 L2420: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L2421;
    }
  goto ret0;

 L2421: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == LABEL_REF)
    goto L2422;
  goto ret0;

 L2422: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  operands[1] = x4;
  goto L2423;

 L2423: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L2424;
  goto ret0;

 L2424: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == LABEL_REF)
    goto L2425;
  goto ret0;

 L2425: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1])
      && (!(Pmode == DImode) && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1]))
    {
      return 297;
    }
  goto ret0;

 L2454: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == PLUS)
    goto L2455;
  goto ret0;

 L2455: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SImode
      && GET_CODE (x4) == MULT)
    goto L2456;
  goto ret0;

 L2456: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[0] = x5;
      goto L2457;
    }
  goto ret0;

 L2457: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 4LL)
    goto L2458;
  goto ret0;

 L2458: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == LABEL_REF)
    goto L2459;
  goto ret0;

 L2459: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  operands[1] = x5;
  goto L2460;

 L2460: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2461;
  goto ret0;

 L2461: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2462;
    }
  goto ret0;

 L2462: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_EMBEDDED_PIC)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 299;
    }
  goto ret0;

 L2404: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L2405;
  goto ret0;

 L2405: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == LABEL_REF)
    goto L2406;
  goto ret0;

 L2406: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L2407;

 L2407: ATTRIBUTE_UNUSED_LABEL
  if ((!(Pmode == DImode)))
    {
      return 295;
    }
  goto ret0;

 L3732: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2430;
    case MEM:
      goto L2482;
    case SUBREG:
    case REG:
    case SIGN_EXTEND:
      goto L3730;
    default:
      goto ret0;
   }
 L3730: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2412;
    }
  goto ret0;

 L2430: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2431;
    }
  goto ret0;

 L2431: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == LABEL_REF)
    goto L2432;
  goto ret0;

 L2432: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  operands[1] = x4;
  goto L2433;

 L2433: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L2434;
  goto ret0;

 L2434: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == LABEL_REF)
    goto L2435;
  goto ret0;

 L2435: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1])
      && (Pmode == DImode && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1]))
    {
      return 298;
    }
  goto ret0;

 L2482: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L2483;
  goto ret0;

 L2483: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == SIGN_EXTEND)
    goto L2484;
  goto ret0;

 L2484: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == SImode
      && GET_CODE (x5) == MULT)
    goto L2485;
  goto ret0;

 L2485: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[0] = x6;
      goto L2486;
    }
  goto ret0;

 L2486: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (GET_CODE (x6) == CONST_INT
      && XWINT (x6, 0) == 8LL)
    goto L2487;
  goto ret0;

 L2487: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == LABEL_REF)
    goto L2488;
  goto ret0;

 L2488: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  operands[1] = x5;
  goto L2489;

 L2489: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2490;
  goto ret0;

 L2490: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2491;
    }
  goto ret0;

 L2491: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_EMBEDDED_PIC)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 300;
    }
  goto ret0;

 L2412: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L2413;
  goto ret0;

 L2413: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == LABEL_REF)
    goto L2414;
  goto ret0;

 L2414: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  goto L2415;

 L2415: ATTRIBUTE_UNUSED_LABEL
  if ((Pmode == DImode))
    {
      return 296;
    }
  goto ret0;

 L2502: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case UNSPEC:
      goto L3735;
    case CALL:
      goto L2599;
    default:
     break;
   }
  goto ret0;

 L3735: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 4)
    goto L2503;
  goto ret0;

 L2503: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  operands[1] = x3;
  goto L2504;

 L2504: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2505;
  goto ret0;

 L2505: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 31
      && (TARGET_EMBEDDED_PIC
   && GET_CODE (operands[1]) == SYMBOL_REF))
    {
      return 304;
    }
  goto ret0;

 L2599: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == MEM)
    goto L2600;
 L2629: ATTRIBUTE_UNUSED_LABEL
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L3736;
    case DImode:
      goto L3737;
    default:
      break;
    }
  goto ret0;

 L2600: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_insn_operand (x4, VOIDmode))
    {
      operands[1] = x4;
      goto L2601;
    }
  goto L2629;

 L2601: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  operands[2] = x3;
  goto L2602;

 L2602: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2603;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L2629;

 L2603: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2604;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L2629;

 L2604: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31))
    {
      return 316;
    }
 L2614: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_ABICALLS && !TARGET_LONG_CALLS))
    {
      return 317;
    }
 L2624: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_ABICALLS && !TARGET_LONG_CALLS))
    {
      return 318;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L2629;

 L3736: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MEM)
    goto L2630;
  goto ret0;

 L2630: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L2631;
    }
  goto ret0;

 L2631: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  operands[2] = x3;
  goto L2632;

 L2632: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2633;
  goto ret0;

 L2633: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2634;
    }
  goto ret0;

 L2634: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 319;
    }
 L2654: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31))
    {
      return 321;
    }
 L2664: ATTRIBUTE_UNUSED_LABEL
  if ((!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 322;
    }
  goto ret0;

 L3737: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MEM)
    goto L2640;
  goto ret0;

 L2640: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (se_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L2641;
    }
  goto ret0;

 L2641: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  operands[2] = x3;
  goto L2642;

 L2642: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2643;
  goto ret0;

 L2643: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2644;
    }
  goto ret0;

 L2644: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 320;
    }
 L2674: ATTRIBUTE_UNUSED_LABEL
  if ((Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 323;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_7 PARAMS ((rtx, rtx, int *));
static int
recog_7 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L371;
    case NEG:
      goto L447;
    case MINUS:
      goto L470;
    case TRUNCATE:
      goto L579;
    case PLUS:
      goto L673;
    case DIV:
      goto L812;
    case UDIV:
      goto L862;
    case MOD:
      goto L958;
    case UMOD:
      goto L1026;
    case FFS:
      goto L1102;
    default:
     break;
   }
  goto ret0;

 L371: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L3769;
  goto ret0;

 L3769: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L372;
    }
 L3770: ATTRIBUTE_UNUSED_LABEL
  if (extend_operator (x3, DImode))
    {
      operands[3] = x3;
      goto L427;
    }
  goto ret0;

 L372: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L373;
    }
  x3 = XEXP (x2, 0);
  goto L3770;

 L373: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L374;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L3770;

 L374: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L375;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L3770;

 L375: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L376;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L3770;

 L376: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L377;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L3770;

 L377: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS4000 && !TARGET_MIPS16))
    {
      return 41;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L3770;

 L427: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L428;
    }
  goto ret0;

 L428: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      operands[4] = x3;
      goto L429;
    }
  goto ret0;

 L429: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L430;
    }
  goto ret0;

 L430: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L431;
  goto ret0;

 L431: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[5] = x2;
      goto L432;
    }
  goto ret0;

 L432: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L433;
  goto ret0;

 L433: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[6] = x2;
      goto L434;
    }
  goto ret0;

 L434: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 44;
    }
  goto ret0;

 L447: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L448;
  goto ret0;

 L448: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      operands[3] = x4;
      goto L449;
    }
  goto ret0;

 L449: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[1] = x5;
      goto L450;
    }
  goto ret0;

 L450: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      operands[4] = x4;
      goto L451;
    }
  goto ret0;

 L451: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L452;
    }
  goto ret0;

 L452: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L453;
  goto ret0;

 L453: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L454;
    }
  goto ret0;

 L454: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L455;
  goto ret0;

 L455: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L456;
    }
  goto ret0;

 L456: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT
   && ISA_HAS_MULS
   && GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 45;
    }
  goto ret0;

 L470: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L471;
    }
  goto ret0;

 L471: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L472;
  goto ret0;

 L472: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      operands[4] = x4;
      goto L473;
    }
  goto ret0;

 L473: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[1] = x5;
      goto L474;
    }
  goto ret0;

 L474: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      operands[5] = x4;
      goto L475;
    }
  goto ret0;

 L475: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L476;
    }
  goto ret0;

 L476: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L477;
  goto ret0;

 L477: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L478;
    }
  goto ret0;

 L478: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L479;
  goto ret0;

 L479: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L480;
    }
  goto ret0;

 L480: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT
   && ISA_HAS_MSAC
   && GET_CODE (operands[4]) == GET_CODE (operands[5])))
    {
      return 46;
    }
  goto ret0;

 L579: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode
      && GET_CODE (x3) == LSHIFTRT)
    goto L580;
  goto ret0;

 L580: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == MULT)
    goto L581;
  goto ret0;

 L581: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TImode)
    goto L3771;
  goto ret0;

 L3771: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x5))
    {
    case SIGN_EXTEND:
      goto L582;
    case ZERO_EXTEND:
      goto L608;
    default:
     break;
   }
  goto ret0;

 L582: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L583;
    }
  goto ret0;

 L583: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode
      && GET_CODE (x5) == SIGN_EXTEND)
    goto L584;
  goto ret0;

 L584: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L585;
    }
  goto ret0;

 L585: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 64LL)
    goto L586;
  goto ret0;

 L586: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L587;
  goto ret0;

 L587: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L588;
    }
  goto ret0;

 L588: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L589;
  goto ret0;

 L589: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L590;
    }
  goto ret0;

 L590: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 50;
    }
  goto ret0;

 L608: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L609;
    }
  goto ret0;

 L609: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode
      && GET_CODE (x5) == ZERO_EXTEND)
    goto L610;
  goto ret0;

 L610: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L611;
    }
  goto ret0;

 L611: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 64LL)
    goto L612;
  goto ret0;

 L612: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L613;
  goto ret0;

 L613: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L614;
    }
  goto ret0;

 L614: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L615;
  goto ret0;

 L615: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L616;
    }
  goto ret0;

 L616: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 51;
    }
  goto ret0;

 L673: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L674;
  goto ret0;

 L674: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      operands[3] = x4;
      goto L675;
    }
  goto ret0;

 L675: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[1] = x5;
      goto L676;
    }
  goto ret0;

 L676: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      operands[4] = x4;
      goto L677;
    }
  goto ret0;

 L677: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L678;
    }
  goto ret0;

 L678: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[0]))
    goto L679;
  goto ret0;

 L679: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L680;
  goto ret0;

 L680: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L681;
    }
  goto ret0;

 L681: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L682;
  goto ret0;

 L682: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L683;
    }
  goto ret0;

 L683: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MAD
   && TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 54;
    }
  goto ret0;

 L812: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L813;
    }
  goto ret0;

 L813: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L814;
    }
 L925: ATTRIBUTE_UNUSED_LABEL
  if (se_nonmemory_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L926;
    }
  goto ret0;

 L814: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L815;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L815: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L816;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L816: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MOD)
    goto L817;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L817: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L818;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L818: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2]))
    goto L819;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L819: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L820;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L820: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L821;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L821: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && optimize))
    {
      return 68;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L925;

 L926: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L927;
  goto ret0;

 L927: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L928;
    }
  goto ret0;

 L928: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L929;
  goto ret0;

 L929: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L930;
    }
  goto ret0;

 L930: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize))
    {
      return 74;
    }
  goto ret0;

 L862: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L863;
    }
  goto ret0;

 L863: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L864;
    }
 L993: ATTRIBUTE_UNUSED_LABEL
  if (se_nonmemory_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L994;
    }
  goto ret0;

 L864: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L865;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L865: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L866;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L866: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UMOD)
    goto L867;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L867: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L868;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L868: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2]))
    goto L869;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L869: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L870;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L870: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L871;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L871: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && optimize))
    {
      return 70;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L993;

 L994: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L995;
  goto ret0;

 L995: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L996;
    }
  goto ret0;

 L996: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L997;
  goto ret0;

 L997: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L998;
    }
  goto ret0;

 L998: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize))
    {
      return 78;
    }
  goto ret0;

 L958: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L959;
    }
  goto ret0;

 L959: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_nonmemory_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L960;
    }
  goto ret0;

 L960: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L961;
  goto ret0;

 L961: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L962;
    }
  goto ret0;

 L962: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L963;
  goto ret0;

 L963: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L964;
    }
  goto ret0;

 L964: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize))
    {
      return 76;
    }
  goto ret0;

 L1026: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1027;
    }
  goto ret0;

 L1027: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_nonmemory_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1028;
    }
  goto ret0;

 L1028: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1029;
  goto ret0;

 L1029: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1030;
    }
  goto ret0;

 L1030: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1031;
  goto ret0;

 L1031: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1032;
    }
  goto ret0;

 L1032: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !optimize))
    {
      return 80;
    }
  goto ret0;

 L1102: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1103;
    }
  goto ret0;

 L1103: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1104;
  goto ret0;

 L1104: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1105;
    }
  goto ret0;

 L1105: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1106;
  goto ret0;

 L1106: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1107;
    }
  goto ret0;

 L1107: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && !TARGET_MIPS16))
    {
      return 90;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_8 PARAMS ((rtx, rtx, int *));
static int
recog_8 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3749;
    case DImode:
      goto L3750;
    default:
      break;
    }
 L2438: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PC)
    goto L2439;
  if (register_operand (x2, VOIDmode))
    {
      operands[0] = x2;
      goto L2678;
    }
  goto ret0;

 L3749: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L238;
    }
  goto L2438;

 L238: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L3751;
  x2 = XEXP (x1, 0);
  goto L2438;

 L3751: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L239;
    case TRUNCATE:
      goto L495;
    case PLUS:
      goto L631;
    case DIV:
      goto L787;
    case UDIV:
      goto L837;
    case MOD:
      goto L941;
    case UMOD:
      goto L1009;
    case FFS:
      goto L1087;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L2438;

 L239: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L240;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L240: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L241;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L241: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L242;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L242: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L243;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L243: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L244;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L244: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L245;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L245: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS4000 || TARGET_MIPS16))
    {
      return 35;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L495: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (highpart_shift_operator (x3, DImode))
    {
      operands[5] = x3;
      goto L496;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L496: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L497;
  x2 = XEXP (x1, 0);
  goto L2438;

 L497: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (extend_operator (x5, DImode))
    {
      operands[3] = x5;
      goto L498;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L498: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[1] = x6;
      goto L499;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L499: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (extend_operator (x5, DImode))
    {
      operands[4] = x5;
      goto L500;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L500: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[2] = x6;
      goto L501;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L501: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 32LL)
    goto L502;
  x2 = XEXP (x1, 0);
  goto L2438;

 L502: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L503;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L503: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L504;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L504: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L505;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L505: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L506;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L506: ATTRIBUTE_UNUSED_LABEL
  if ((GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 47;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L631: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L632;
  x2 = XEXP (x1, 0);
  goto L2438;

 L632: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L633;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L633: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L634;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L634: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[0]))
    goto L635;
  x2 = XEXP (x1, 0);
  goto L2438;

 L635: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L636;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L636: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L637;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L637: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L638;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L638: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L639;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L639: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MAD))
    {
      return 52;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L787: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L788;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L788: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L789;
    }
 L908: ATTRIBUTE_UNUSED_LABEL
  if (nonmemory_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L909;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L789: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L790;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L790: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L791;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L791: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MOD)
    goto L792;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L792: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L793;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L793: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2]))
    goto L794;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L794: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L795;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L795: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L796;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L796: ATTRIBUTE_UNUSED_LABEL
  if ((optimize))
    {
      return 67;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L908;

 L909: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L910;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L910: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L911;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L911: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L912;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L912: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L913;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L913: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize))
    {
      return 73;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L837: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L838;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L838: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L839;
    }
 L976: ATTRIBUTE_UNUSED_LABEL
  if (nonmemory_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L977;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L839: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L840;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L840: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L841;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L841: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == UMOD)
    goto L842;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L842: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L843;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L843: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2]))
    goto L844;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L844: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L845;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L845: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L846;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L846: ATTRIBUTE_UNUSED_LABEL
  if ((optimize))
    {
      return 69;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L976;

 L977: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L978;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L978: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L979;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L979: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L980;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L980: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L981;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L981: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize))
    {
      return 77;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L941: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L942;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L942: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L943;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L943: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L944;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L944: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L945;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L945: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L946;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L946: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L947;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L947: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize))
    {
      return 75;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1009: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1010;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L1010: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1011;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L1011: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1012;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1012: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1013;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1013: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1014;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1014: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1015;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1015: ATTRIBUTE_UNUSED_LABEL
  if ((!optimize))
    {
      return 79;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1087: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1088;
    }
  x2 = XEXP (x1, 0);
  goto L2438;

 L1088: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1089;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1089: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1090;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1090: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1091;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1091: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1092;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L1092: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 89;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2438;

 L3750: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L370;
    }
  goto L2438;

 L370: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L3759;
  x2 = XEXP (x1, 0);
  goto L2438;

 L3759: ATTRIBUTE_UNUSED_LABEL
  tem = recog_7 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x2 = XEXP (x1, 0);
  goto L2438;

 L2439: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3773;
    case DImode:
      goto L3774;
    default:
      break;
    }
  goto ret0;

 L3773: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MEM)
    goto L2440;
  goto ret0;

 L2440: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == PLUS)
    goto L2441;
  goto ret0;

 L2441: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SImode
      && GET_CODE (x4) == MULT)
    goto L2442;
  goto ret0;

 L2442: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      operands[0] = x5;
      goto L2443;
    }
  goto ret0;

 L2443: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 4LL)
    goto L2444;
  goto ret0;

 L2444: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == LABEL_REF)
    goto L2445;
  goto ret0;

 L2445: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  operands[1] = x5;
  goto L2446;

 L2446: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2447;
  goto ret0;

 L2447: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2448;
    }
  goto ret0;

 L2448: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2449;
  goto ret0;

 L2449: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 31
      && (TARGET_EMBEDDED_PIC))
    {
      return 299;
    }
  goto ret0;

 L3774: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MEM)
    goto L2467;
  goto ret0;

 L2467: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L2468;
  goto ret0;

 L2468: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == SIGN_EXTEND)
    goto L2469;
  goto ret0;

 L2469: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == SImode
      && GET_CODE (x5) == MULT)
    goto L2470;
  goto ret0;

 L2470: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[0] = x6;
      goto L2471;
    }
  goto ret0;

 L2471: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (GET_CODE (x6) == CONST_INT
      && XWINT (x6, 0) == 8LL)
    goto L2472;
  goto ret0;

 L2472: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == LABEL_REF)
    goto L2473;
  goto ret0;

 L2473: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  operands[1] = x5;
  goto L2474;

 L2474: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2475;
  goto ret0;

 L2475: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2476;
    }
  goto ret0;

 L2476: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2477;
  goto ret0;

 L2477: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 31
      && (TARGET_EMBEDDED_PIC))
    {
      return 300;
    }
  goto ret0;

 L2678: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L2679;
  goto ret0;

 L2679: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == MEM)
    goto L2680;
  goto ret0;

 L2680: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_insn_operand (x4, VOIDmode))
    {
      operands[1] = x4;
      goto L2681;
    }
  goto ret0;

 L2681: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  operands[2] = x3;
  goto L2682;

 L2682: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L2683;
  goto ret0;

 L2683: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2684;
    }
  goto ret0;

 L2684: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L2685;
  goto ret0;

 L2685: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == MEM)
    goto L2686;
  goto ret0;

 L2686: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L2687;
  goto ret0;

 L2687: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[2]))
    goto L2688;
  goto ret0;

 L2688: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2689;
  goto ret0;

 L2689: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L2690;
    }
  goto ret0;

 L2690: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_ABICALLS && !TARGET_LONG_CALLS))
    {
      return 324;
    }
 L2706: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_ABICALLS && !TARGET_LONG_CALLS))
    {
      return 325;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_9 PARAMS ((rtx, rtx, int *));
static int
recog_9 (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  switch (XVECLEN (x0, 0))
    {
    case 2:
      goto L45;
    case 4:
      goto L217;
    case 3:
      goto L236;
    case 5:
      goto L272;
    case 8:
      goto L1655;
    default:
      break;
    }
  goto ret0;

 L45: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L46;
    case TRAP_IF:
      goto L891;
    case USE:
      goto L2497;
    case UNSPEC:
      goto L3709;
    case CALL:
      goto L2533;
    default:
     break;
   }
  goto ret0;

 L46: ATTRIBUTE_UNUSED_LABEL
  return recog_6 (x0, insn, pnum_clobbers);

 L891: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == EQ)
    goto L892;
  goto ret0;

 L892: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L893;
    }
  goto ret0;

 L893: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (true_reg_or_0_operand (x3, VOIDmode))
    {
      operands[1] = x3;
      goto L894;
    }
  goto ret0;

 L894: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, VOIDmode))
    {
      operands[2] = x2;
      goto L895;
    }
  goto ret0;

 L895: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L896;
  goto ret0;

 L896: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 24
      && (TARGET_MIPS16))
    {
      return 72;
    }
  goto ret0;

 L2497: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (pmode_register_operand (x2, VOIDmode))
    {
      operands[0] = x2;
      goto L2498;
    }
  goto ret0;

 L2498: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == RETURN)
    {
      return 303;
    }
  goto ret0;

 L3709: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 11)
    goto L2513;
  goto ret0;

 L2513: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3738;
    case DImode:
      goto L3739;
    default:
      break;
    }
  goto ret0;

 L3738: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L2514;
    }
  goto ret0;

 L2514: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2515;
  goto ret0;

 L2515: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2516;
    }
  goto ret0;

 L2516: ATTRIBUTE_UNUSED_LABEL
  if ((! TARGET_64BIT))
    {
      return 305;
    }
  goto ret0;

 L3739: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2523;
    }
  goto ret0;

 L2523: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2524;
  goto ret0;

 L2524: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2525;
    }
  goto ret0;

 L2525: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT))
    {
      return 306;
    }
  goto ret0;

 L2533: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == MEM)
    goto L2534;
 L2557: ATTRIBUTE_UNUSED_LABEL
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3740;
    case DImode:
      goto L3741;
    default:
      break;
    }
  goto ret0;

 L2534: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_insn_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L2535;
    }
  goto L2557;

 L2535: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  operands[1] = x2;
  goto L2536;

 L2536: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2537;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2557;

 L2537: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2538;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2557;

 L2538: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31))
    {
      return 308;
    }
 L2546: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_ABICALLS && !TARGET_LONG_CALLS))
    {
      return 309;
    }
 L2554: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_ABICALLS && !TARGET_LONG_CALLS))
    {
      return 310;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2557;

 L3740: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MEM)
    goto L2558;
  goto ret0;

 L2558: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L2559;
    }
  goto ret0;

 L2559: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  operands[1] = x2;
  goto L2560;

 L2560: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2561;
  goto ret0;

 L2561: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2562;
    }
  goto ret0;

 L2562: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 311;
    }
 L2578: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31))
    {
      return 313;
    }
 L2586: ATTRIBUTE_UNUSED_LABEL
  if ((!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 314;
    }
  goto ret0;

 L3741: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MEM)
    goto L2566;
  goto ret0;

 L2566: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2567;
    }
  goto ret0;

 L2567: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  operands[1] = x2;
  goto L2568;

 L2568: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2569;
  goto ret0;

 L2569: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2570;
    }
  goto ret0;

 L2570: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 312;
    }
 L2594: ATTRIBUTE_UNUSED_LABEL
  if ((Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS))
    {
      return 315;
    }
  goto ret0;

 L217: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L218;
  goto ret0;

 L218: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L3742;
    case DImode:
      goto L3743;
    default:
      break;
    }
  goto ret0;

 L3742: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L219;
    }
  goto ret0;

 L219: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L3744;
  goto ret0;

 L3744: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L220;
    case NEG:
      goto L325;
    case TRUNCATE:
      goto L521;
    default:
     break;
   }
  goto ret0;

 L220: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L221;
    }
  goto ret0;

 L221: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L222;
    }
  goto ret0;

 L222: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L223;
  goto ret0;

 L223: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L224;
    }
  goto ret0;

 L224: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L225;
  goto ret0;

 L225: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L226;
    }
  goto ret0;

 L226: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L227;
  goto ret0;

 L227: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L228;
    }
  goto ret0;

 L228: ATTRIBUTE_UNUSED_LABEL
  if ((GENERATE_MULT3_SI
   || TARGET_MAD))
    {
      return 34;
    }
 L264: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS4000 && !TARGET_MIPS16))
    {
      return 36;
    }
  goto ret0;

 L325: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L326;
  goto ret0;

 L326: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L327;
    }
  goto ret0;

 L327: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L328;
    }
  goto ret0;

 L328: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L329;
  goto ret0;

 L329: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L330;
    }
  goto ret0;

 L330: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L331;
  goto ret0;

 L331: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L332;
    }
  goto ret0;

 L332: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L333;
  goto ret0;

 L333: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L334;
    }
  goto ret0;

 L334: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MULS && TARGET_64BIT))
    {
      return 39;
    }
  goto ret0;

 L521: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (highpart_shift_operator (x3, DImode))
    {
      operands[5] = x3;
      goto L522;
    }
  goto ret0;

 L522: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode)
    goto L3747;
  goto ret0;

 L3747: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L523;
    case NEG:
      goto L551;
    default:
     break;
   }
  goto ret0;

 L523: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (extend_operator (x5, DImode))
    {
      operands[3] = x5;
      goto L524;
    }
  goto ret0;

 L524: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[1] = x6;
      goto L525;
    }
  goto ret0;

 L525: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (extend_operator (x5, DImode))
    {
      operands[4] = x5;
      goto L526;
    }
  goto ret0;

 L526: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      operands[2] = x6;
      goto L527;
    }
  goto ret0;

 L527: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 32LL)
    goto L528;
  goto ret0;

 L528: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L529;
  goto ret0;

 L529: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L530;
    }
  goto ret0;

 L530: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L531;
  goto ret0;

 L531: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L532;
    }
  goto ret0;

 L532: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L533;
  goto ret0;

 L533: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[8] = x2;
      goto L534;
    }
  goto ret0;

 L534: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MULHI
   && TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 48;
    }
  goto ret0;

 L551: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DImode
      && GET_CODE (x5) == MULT)
    goto L552;
  goto ret0;

 L552: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (extend_operator (x6, DImode))
    {
      operands[3] = x6;
      goto L553;
    }
  goto ret0;

 L553: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 0);
  if (register_operand (x7, SImode))
    {
      operands[1] = x7;
      goto L554;
    }
  goto ret0;

 L554: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (extend_operator (x6, DImode))
    {
      operands[4] = x6;
      goto L555;
    }
  goto ret0;

 L555: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 0);
  if (register_operand (x7, SImode))
    {
      operands[2] = x7;
      goto L556;
    }
  goto ret0;

 L556: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 32LL)
    goto L557;
  goto ret0;

 L557: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L558;
  goto ret0;

 L558: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L559;
    }
  goto ret0;

 L559: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L560;
  goto ret0;

 L560: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L561;
    }
  goto ret0;

 L561: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L562;
  goto ret0;

 L562: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[8] = x2;
      goto L563;
    }
  goto ret0;

 L563: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MULHI
   && TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4])))
    {
      return 49;
    }
  goto ret0;

 L3743: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L387;
    }
  goto ret0;

 L387: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MULT)
    goto L388;
  goto ret0;

 L388: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L389;
    }
  goto ret0;

 L389: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L390;
    }
  goto ret0;

 L390: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L391;
  goto ret0;

 L391: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L392;
    }
  goto ret0;

 L392: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L393;
  goto ret0;

 L393: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L394;
    }
  goto ret0;

 L394: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L395;
  goto ret0;

 L395: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[5] = x2;
      goto L396;
    }
  goto ret0;

 L396: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && (GENERATE_MULT3_DI || TARGET_MIPS4000 || TARGET_MIPS16)))
    {
      return 42;
    }
  goto ret0;

 L236: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L237;
  goto ret0;

 L237: ATTRIBUTE_UNUSED_LABEL
  return recog_8 (x0, insn, pnum_clobbers);

 L272: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L273;
  goto ret0;

 L273: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L274;
    }
  goto ret0;

 L274: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L3775;
  goto ret0;

 L3775: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L275;
    case MINUS:
      goto L300;
    default:
     break;
   }
  goto ret0;

 L275: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L276;
  goto ret0;

 L276: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L277;
    }
  goto ret0;

 L277: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L278;
    }
  goto ret0;

 L278: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L279;
    }
  goto ret0;

 L279: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L280;
  goto ret0;

 L280: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L281;
    }
  goto ret0;

 L281: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L282;
  goto ret0;

 L282: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L283;
    }
  goto ret0;

 L283: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L284;
  goto ret0;

 L284: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L285;
    }
  goto ret0;

 L285: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L286;
  goto ret0;

 L286: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L287;
    }
  goto ret0;

 L287: ATTRIBUTE_UNUSED_LABEL
  if (((TARGET_MIPS3900
   || TARGET_MIPS5400
   || TARGET_MIPS5500
   || ISA_HAS_MADD_MSUB)
   && !TARGET_MIPS16))
    {
      return 37;
    }
  goto ret0;

 L300: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L301;
    }
  goto ret0;

 L301: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L302;
  goto ret0;

 L302: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L303;
    }
  goto ret0;

 L303: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L304;
    }
  goto ret0;

 L304: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L305;
  goto ret0;

 L305: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L306;
    }
  goto ret0;

 L306: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L307;
  goto ret0;

 L307: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L308;
    }
  goto ret0;

 L308: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L309;
  goto ret0;

 L309: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L310;
    }
  goto ret0;

 L310: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L311;
  goto ret0;

 L311: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L312;
    }
  goto ret0;

 L312: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MADD_MSUB))
    {
      return 38;
    }
 L358: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_MSAC && TARGET_64BIT))
    {
      return 40;
    }
  goto ret0;

 L1655: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L1656;
  goto ret0;

 L1656: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (memory_operand (x2, BLKmode))
    {
      operands[0] = x2;
      goto L1657;
    }
  goto ret0;

 L1657: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (memory_operand (x2, BLKmode))
    {
      operands[1] = x2;
      goto L1658;
    }
  goto ret0;

 L1658: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1659;
  goto ret0;

 L1659: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1660;
    }
  goto ret0;

 L1660: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1661;
  goto ret0;

 L1661: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L1662;
    }
  goto ret0;

 L1662: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1663;
  goto ret0;

 L1663: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L1664;
    }
  goto ret0;

 L1664: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L1665;
  goto ret0;

 L1665: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L1666;
    }
  goto ret0;

 L1666: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == USE)
    goto L1667;
  goto ret0;

 L1667: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (small_int (x2, SImode))
    {
      operands[2] = x2;
      goto L1668;
    }
  goto ret0;

 L1668: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 6);
  if (GET_CODE (x1) == USE)
    goto L1669;
  goto ret0;

 L1669: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (small_int (x2, SImode))
    {
      operands[3] = x2;
      goto L1670;
    }
  goto ret0;

 L1670: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 7);
  if (GET_CODE (x1) == USE)
    goto L1671;
  goto ret0;

 L1671: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT)
    goto L3777;
  goto ret0;

 L3777: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x2, 0) == XWINT (x2, 0))
    switch ((int) XWINT (x2, 0))
      {
      case 0LL:
        goto L3782;
      case 1LL:
        goto L3784;
      case 2LL:
        goto L3786;
      default:
        break;
      }
  goto ret0;

 L3782: ATTRIBUTE_UNUSED_LABEL
  return 190;
 L3783: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 191;
    }
  goto ret0;

 L3784: ATTRIBUTE_UNUSED_LABEL
  return 192;
 L3785: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 193;
    }
  goto ret0;

 L3786: ATTRIBUTE_UNUSED_LABEL
  return 194;
 ret0:
  return -1;
}

int recog PARAMS ((rtx, rtx, int *));
int
recog (x0, insn, pnum_clobbers)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;
  recog_data.insn = NULL_RTX;

  switch (GET_CODE (x0))
    {
    case TRAP_IF:
      goto L1;
    case SET:
      goto L9;
    case PARALLEL:
      goto L3404;
    case UNSPEC:
      goto L3408;
    case UNSPEC_VOLATILE:
      goto L3410;
    case RETURN:
      goto L3411;
    case PREFETCH:
      goto L2708;
    case CONST_INT:
      goto L3414;
    default:
     break;
   }
  goto ret0;

 L1: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 1LL)
    goto L2;
  if (trap_cmp_op (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L5;
    }
 L884: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == EQ)
    goto L885;
  goto ret0;

 L2: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL)
    {
      return 0;
    }
  goto ret0;

 L5: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (reg_or_0_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L6;
    }
  goto L884;

 L6: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L7;
    }
  goto L884;

 L7: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL
      && (ISA_HAS_COND_TRAP))
    {
      return 1;
    }
  x1 = XEXP (x0, 0);
  goto L884;

 L885: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, VOIDmode))
    {
      operands[0] = x2;
      goto L886;
    }
  goto ret0;

 L886: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (true_reg_or_0_operand (x2, VOIDmode))
    {
      operands[1] = x2;
      goto L887;
    }
  goto ret0;

 L887: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (immediate_operand (x1, VOIDmode))
    {
      operands[2] = x1;
      goto L888;
    }
  goto ret0;

 L888: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_MIPS16))
    {
      return 71;
    }
 L902: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 72;
    }
  goto ret0;

 L9: ATTRIBUTE_UNUSED_LABEL
  return recog_5 (x0, insn, pnum_clobbers);

 L3404: ATTRIBUTE_UNUSED_LABEL
  return recog_9 (x0, insn, pnum_clobbers);

 L3408: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1)
    goto L3787;
  goto ret0;

 L3787: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x0, 1))
    {
    case 5LL:
      goto L1535;
    case 11LL:
      goto L2518;
    default:
      break;
    }
  goto ret0;

 L1535: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (register_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      return 168;
    }
  goto ret0;

 L2518: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_MODE (x1))
    {
    case SImode:
      goto L3789;
    case DImode:
      goto L3790;
    default:
      break;
    }
  goto ret0;

 L3789: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2519;
    }
  goto ret0;

 L2519: ATTRIBUTE_UNUSED_LABEL
  if ((! TARGET_64BIT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 305;
    }
  goto ret0;

 L3790: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2528;
    }
  goto ret0;

 L2528: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 306;
    }
  goto ret0;

 L3410: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1)
    goto L3791;
  goto ret0;

 L3791: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x0, 1))
    {
    case 6LL:
      goto L2493;
    case 10LL:
      goto L2530;
    case 12LL:
      goto L2841;
    case 13LL:
      goto L2844;
    case 14LL:
      goto L2847;
    case 15LL:
      goto L2850;
    case 16LL:
      goto L2853;
    case 17LL:
      goto L2856;
    case 18LL:
      goto L2859;
    case 19LL:
      goto L2861;
    case 20LL:
      goto L2863;
    default:
      break;
    }
  goto ret0;

 L2493: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL)
    {
      return 301;
    }
  goto ret0;

 L2530: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL
      && (TARGET_ABICALLS && (mips_abi == ABI_32 || mips_abi == ABI_O64)))
    {
      return 307;
    }
  goto ret0;

 L2841: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, QImode))
    {
      operands[0] = x1;
      goto L2842;
    }
  goto ret0;

 L2842: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 343;
    }
  goto ret0;

 L2844: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L2845;
    }
  goto ret0;

 L2845: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 344;
    }
  goto ret0;

 L2847: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2848;
    }
  goto ret0;

 L2848: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 345;
    }
  goto ret0;

 L2850: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2851;
    }
  goto ret0;

 L2851: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 346;
    }
  goto ret0;

 L2853: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L2854;
    }
  goto ret0;

 L2854: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 347;
    }
  goto ret0;

 L2856: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L2857;
    }
  goto ret0;

 L2857: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16))
    {
      return 348;
    }
  goto ret0;

 L2859: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL
      && (TARGET_MIPS16))
    {
      return 349;
    }
  goto ret0;

 L2861: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL
      && (TARGET_MIPS16))
    {
      return 350;
    }
  goto ret0;

 L2863: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL
      && (TARGET_MIPS16))
    {
      return 351;
    }
  goto ret0;

 L3411: ATTRIBUTE_UNUSED_LABEL
  if ((mips_can_use_return_insn ()))
    {
      return 302;
    }
  goto ret0;

 L2708: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  switch (GET_MODE (x1))
    {
    case SImode:
      goto L3802;
    case DImode:
      goto L3804;
    default:
      break;
    }
  goto ret0;

 L3802: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == PLUS)
    goto L2709;
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2716;
    }
  goto ret0;

 L2709: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L2710;
    }
  goto ret0;

 L2710: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2711;
    }
  goto ret0;

 L2711: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, SImode))
    {
      operands[1] = x1;
      goto L2712;
    }
  goto ret0;

 L2712: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 2);
  if (const_int_operand (x1, SImode))
    {
      operands[2] = x1;
      goto L2713;
    }
  goto ret0;

 L2713: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_PREFETCH && Pmode == SImode))
    {
      return 326;
    }
  goto ret0;

 L2716: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, SImode))
    {
      operands[1] = x1;
      goto L2717;
    }
  goto ret0;

 L2717: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 2);
  if (const_int_operand (x1, SImode))
    {
      operands[2] = x1;
      goto L2718;
    }
  goto ret0;

 L2718: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_PREFETCH && Pmode == SImode))
    {
      return 327;
    }
  goto ret0;

 L3804: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == PLUS)
    goto L2721;
  if (se_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2728;
    }
  goto ret0;

 L2721: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2722;
    }
  goto ret0;

 L2722: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2723;
    }
  goto ret0;

 L2723: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L2724;
    }
  goto ret0;

 L2724: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 2);
  if (const_int_operand (x1, DImode))
    {
      operands[2] = x1;
      goto L2725;
    }
  goto ret0;

 L2725: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_PREFETCH && Pmode == DImode))
    {
      return 328;
    }
  goto ret0;

 L2728: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L2729;
    }
  goto ret0;

 L2729: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 2);
  if (const_int_operand (x1, DImode))
    {
      operands[2] = x1;
      goto L2730;
    }
  goto ret0;

 L2730: ATTRIBUTE_UNUSED_LABEL
  if ((ISA_HAS_PREFETCH && Pmode == DImode))
    {
      return 329;
    }
  goto ret0;

 L3414: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x0, 0) == 0LL)
    {
      return 330;
    }
  goto ret0;
 ret0:
  return -1;
}

static rtx split_1 PARAMS ((rtx, rtx));
static rtx
split_1 (x0, insn)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  rtx tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 0);
  switch (GET_MODE (x1))
    {
    case SImode:
      goto L3809;
    case DImode:
      goto L3810;
    case HImode:
      goto L3811;
    case QImode:
      goto L3812;
    case DFmode:
      goto L3813;
    default:
      break;
    }
  goto ret0;

 L3809: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2874;
    }
  goto ret0;

 L2874: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L3814;
 L3121: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == CONST_INT)
    goto L3826;
  goto ret0;

 L3814: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L2875;
    case MINUS:
      goto L2935;
    case MEM:
      goto L3126;
    case ASHIFT:
      goto L3179;
    case ASHIFTRT:
      goto L3227;
    case LSHIFTRT:
      goto L3275;
    case EQ:
      goto L3329;
    case NE:
      goto L3341;
    case GE:
      goto L3353;
    case LE:
      goto L3365;
    case GEU:
      goto L3377;
    case LEU:
      goto L3389;
    default:
     break;
   }
  goto L3121;

 L2875: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[0]))
    goto L2876;
 L2881: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2882;
    }
  goto L3121;

 L2876: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2877;
    }
  x2 = XEXP (x1, 0);
  goto L2881;

 L2877: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x7f
	&& INTVAL (operands[1]) <= 0x7f + 0x7f)
       || (INTVAL (operands[1]) < - 0x80
	   && INTVAL (operands[1]) >= - 0x80 - 0x80))))
    {
      return gen_split_356 (operands);
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2881;

 L2882: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2883;
    }
  goto L3121;

 L2883: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0x7f)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x80))))
    {
      return gen_split_357 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L2935: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[0]))
    goto L2936;
 L2941: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2942;
    }
  goto L3121;

 L2936: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2937;
    }
  x2 = XEXP (x1, 0);
  goto L2941;

 L2937: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x80
	&& INTVAL (operands[1]) <= 0x80 + 0x80)
       || (INTVAL (operands[1]) < - 0x7f
	   && INTVAL (operands[1]) >= - 0x7f - 0x7f))))
    {
      return gen_split_366 (operands);
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2941;

 L2942: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2943;
    }
  goto L3121;

 L2943: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x8
	&& INTVAL (operands[2]) <= 0x8 + 0x80)
       || (INTVAL (operands[2]) < - 0x7
	   && INTVAL (operands[2]) >= - 0x7 - 0x7f))))
    {
      return gen_split_367 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3126: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L3127;
  goto L3121;

 L3127: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[0]))
    goto L3128;
  goto L3121;

 L3128: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3129;
    }
  goto L3121;

 L3129: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 4
	   && INTVAL (operands[1]) <= 31 * 4 + 0x7c)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 4
	   && (INTVAL (operands[1]) & 3) != 0))))
    {
      return gen_split_441 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3179: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3180;
    }
  goto L3121;

 L3180: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3181;
    }
  goto L3121;

 L3181: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16))
    {
      return gen_split_458 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3227: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3228;
    }
  goto L3121;

 L3228: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3229;
    }
  goto L3121;

 L3229: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16))
    {
      return gen_split_466 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3275: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L3828;
  goto L3121;

 L3828: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3276;
    }
 L3829: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3282;
    }
  goto L3121;

 L3276: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3277;
    }
  x2 = XEXP (x1, 0);
  goto L3829;

 L3277: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16))
    {
      return gen_split_474 (operands);
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L3829;

 L3282: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3283;
    }
  goto L3121;

 L3283: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && !TARGET_DEBUG_D_MODE))
    {
      return gen_split_475 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3329: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3330;
    }
  goto L3121;

 L3330: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3331;
    }
  goto L3121;

 L3331: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)))
    {
      return gen_split_507 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3341: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3342;
    }
  goto L3121;

 L3342: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3343;
    }
  goto L3121;

 L3343: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)))
    {
      return gen_split_510 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3353: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3354;
    }
  goto L3121;

 L3354: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3355;
    }
  goto L3121;

 L3355: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16))
    {
      return gen_split_514 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3365: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3366;
    }
  goto L3121;

 L3366: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3367;
    }
  goto L3121;

 L3367: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16))
    {
      return gen_split_518 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3377: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3378;
    }
  goto L3121;

 L3378: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3379;
    }
  goto L3121;

 L3379: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16))
    {
      return gen_split_522 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3389: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L3390;
    }
  goto L3121;

 L3390: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3391;
    }
  goto L3121;

 L3391: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16))
    {
      return gen_split_526 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3121;

 L3826: ATTRIBUTE_UNUSED_LABEL
  if (large_int (x1, SImode))
    {
      operands[1] = x1;
      goto L3122;
    }
 L3827: ATTRIBUTE_UNUSED_LABEL
  if (const_int_operand (x1, SImode))
    {
      operands[1] = x1;
      goto L3133;
    }
  goto ret0;

 L3122: ATTRIBUTE_UNUSED_LABEL
  if ((!TARGET_DEBUG_D_MODE && !TARGET_MIPS16))
    {
      return gen_split_439 (operands);
    }
  x1 = XEXP (x0, 1);
  goto L3827;

 L3133: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) >= 0x100
   && INTVAL (operands[1]) <= 0xff + 0x7f))
    {
      return gen_split_442 (operands);
    }
 L3137: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) < 0
   && INTVAL (operands[1]) > - 0x8000))
    {
      return gen_split_443 (operands);
    }
  goto ret0;

 L3810: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2922;
    }
  goto ret0;

 L2922: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L3830;
  goto ret0;

 L3830: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L2923;
    case MINUS:
      goto L2983;
    case NOT:
      goto L3080;
    case AND:
      goto L3085;
    case IOR:
      goto L3091;
    case XOR:
      goto L3097;
    case MEM:
      goto L3115;
    case ASHIFT:
      goto L3221;
    case ASHIFTRT:
      goto L3269;
    case LSHIFTRT:
      goto L3323;
    case EQ:
      goto L3335;
    case NE:
      goto L3347;
    case GE:
      goto L3359;
    case LE:
      goto L3371;
    case GEU:
      goto L3383;
    case LEU:
      goto L3395;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L3836;
    default:
      goto ret0;
   }
 L3836: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L3111;
    }
  goto ret0;

 L2923: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[0]))
    goto L2924;
 L2929: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2930;
    }
  goto ret0;

 L2924: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2925;
    }
  x2 = XEXP (x1, 0);
  goto L2929;

 L2925: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0xf
	&& INTVAL (operands[1]) <= 0xf + 0xf)
       || (INTVAL (operands[1]) < - 0x10
	   && INTVAL (operands[1]) >= - 0x10 - 0x10))))
    {
      return gen_split_363 (operands);
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2929;

 L2930: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2931;
    }
  goto ret0;

 L2931: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0xf)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x10))))
    {
      return gen_split_364 (operands);
    }
  goto ret0;

 L2983: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[0]))
    goto L2984;
 L2989: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2990;
    }
  goto ret0;

 L2984: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2985;
    }
  x2 = XEXP (x1, 0);
  goto L2989;

 L2985: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x10
	&& INTVAL (operands[1]) <= 0x10 + 0x10)
       || (INTVAL (operands[1]) < - 0xf
	   && INTVAL (operands[1]) >= - 0xf - 0xf))))
    {
      return gen_split_373 (operands);
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L2989;

 L2990: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2991;
    }
  goto ret0;

 L2991: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x8
	&& INTVAL (operands[2]) <= 0x8 + 0x10)
       || (INTVAL (operands[2]) < - 0x7
	   && INTVAL (operands[2]) >= - 0x7 - 0xf))))
    {
      return gen_split_374 (operands);
    }
  goto ret0;

 L3080: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3081;
    }
  goto ret0;

 L3081: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))))
    {
      return gen_split_402 (operands);
    }
  goto ret0;

 L3085: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L3848;
  goto ret0;

 L3848: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L3104;
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3086;
    }
  goto ret0;

 L3104: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3105;
    }
  goto ret0;

 L3105: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == NOT)
    goto L3106;
  goto ret0;

 L3106: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L3107;
    }
  goto ret0;

 L3107: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_MIPS16 && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))))
    {
      return gen_split_412 (operands);
    }
  goto ret0;

 L3086: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3087;
    }
  goto ret0;

 L3087: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))))
    {
      return gen_split_405 (operands);
    }
  goto ret0;

 L3091: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3092;
    }
  goto ret0;

 L3092: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3093;
    }
  goto ret0;

 L3093: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))))
    {
      return gen_split_408 (operands);
    }
  goto ret0;

 L3097: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3098;
    }
  goto ret0;

 L3098: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3099;
    }
  goto ret0;

 L3099: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))))
    {
      return gen_split_411 (operands);
    }
  goto ret0;

 L3115: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L3116;
  goto ret0;

 L3116: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[0]))
    goto L3117;
  goto ret0;

 L3117: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3118;
    }
  goto ret0;

 L3118: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_MIPS16 && reload_completed
   && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x10)
       || (INTVAL (operands[1]) >= 32 * 8
	   && INTVAL (operands[1]) <= 31 * 8 + 0x8)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 8
	   && (INTVAL (operands[1]) & 7) != 0))))
    {
      return gen_split_436 (operands);
    }
  goto ret0;

 L3221: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3222;
    }
  goto ret0;

 L3222: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3223;
    }
  goto ret0;

 L3223: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT && !TARGET_DEBUG_D_MODE
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16))
    {
      return gen_split_464 (operands);
    }
  goto ret0;

 L3269: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3270;
    }
  goto ret0;

 L3270: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3271;
    }
  goto ret0;

 L3271: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && TARGET_64BIT && !TARGET_DEBUG_D_MODE
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16))
    {
      return gen_split_472 (operands);
    }
  goto ret0;

 L3323: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3324;
    }
  goto ret0;

 L3324: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L3325;
    }
  goto ret0;

 L3325: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16))
    {
      return gen_split_481 (operands);
    }
  goto ret0;

 L3335: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3336;
    }
  goto ret0;

 L3336: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_uns_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3337;
    }
  goto ret0;

 L3337: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)))
    {
      return gen_split_508 (operands);
    }
  goto ret0;

 L3347: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3348;
    }
  goto ret0;

 L3348: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_uns_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3349;
    }
  goto ret0;

 L3349: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)))
    {
      return gen_split_511 (operands);
    }
  goto ret0;

 L3359: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3360;
    }
  goto ret0;

 L3360: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3361;
    }
  goto ret0;

 L3361: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16))
    {
      return gen_split_515 (operands);
    }
  goto ret0;

 L3371: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3372;
    }
  goto ret0;

 L3372: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3373;
    }
  goto ret0;

 L3373: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16))
    {
      return gen_split_519 (operands);
    }
  goto ret0;

 L3383: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3384;
    }
  goto ret0;

 L3384: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3385;
    }
  goto ret0;

 L3385: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16))
    {
      return gen_split_523 (operands);
    }
  goto ret0;

 L3395: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L3396;
    }
  goto ret0;

 L3396: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L3397;
    }
  goto ret0;

 L3397: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16))
    {
      return gen_split_527 (operands);
    }
  goto ret0;

 L3111: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))))
    {
      return gen_split_435 (operands);
    }
  goto ret0;

 L3811: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L3140;
    }
  goto ret0;

 L3140: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == HImode
      && GET_CODE (x1) == MEM)
    goto L3141;
  goto ret0;

 L3141: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L3142;
  goto ret0;

 L3142: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[0]))
    goto L3143;
  goto ret0;

 L3143: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3144;
    }
  goto ret0;

 L3144: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 2
	   && INTVAL (operands[1]) <= 31 * 2 + 0x7e)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 2
	   && (INTVAL (operands[1]) & 1) != 0))))
    {
      return gen_split_449 (operands);
    }
  goto ret0;

 L3812: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, QImode))
    {
      operands[0] = x1;
      goto L3147;
    }
  goto ret0;

 L3147: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == QImode
      && GET_CODE (x1) == MEM)
    goto L3148;
  goto ret0;

 L3148: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L3149;
  goto ret0;

 L3149: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[0]))
    goto L3150;
  goto ret0;

 L3150: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3151;
    }
  goto ret0;

 L3151: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32
	   && INTVAL (operands[1]) <= 31 + 0x7f))))
    {
      return gen_split_451 (operands);
    }
  goto ret0;

 L3813: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L3154;
    }
  goto ret0;

 L3154: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DFmode))
    {
      operands[1] = x1;
      goto L3155;
    }
  goto ret0;

 L3155: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))))
    {
      return gen_split_454 (operands);
    }
  goto ret0;
 ret0:
  return 0;
}

static rtx split_2 PARAMS ((rtx, rtx));
static rtx
split_2 (x0, insn)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  rtx tem ATTRIBUTE_UNUSED;

  switch (XVECLEN (x0, 0))
    {
    case 2:
      goto L2885;
    case 5:
      goto L2993;
    case 8:
      goto L3157;
    default:
      break;
    }
  goto ret0;

 L2885: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L2886;
    case UNSPEC:
      goto L3849;
    default:
     break;
   }
  goto ret0;

 L2886: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2887;
    }
  goto ret0;

 L2887: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L3850;
  goto ret0;

 L3850: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2888;
    case MINUS:
      goto L2948;
    case ASHIFT:
      goto L3186;
    case ASHIFTRT:
      goto L3234;
    case LSHIFTRT:
      goto L3288;
    default:
     break;
   }
  goto ret0;

 L2888: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2889;
    }
  goto ret0;

 L2889: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2890;
    }
  if (small_int (x3, DImode))
    {
      operands[2] = x3;
      goto L2908;
    }
  goto ret0;

 L2890: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2891;
  goto ret0;

 L2891: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2892;
    }
  goto ret0;

 L2892: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1])
       || REGNO (operands[0]) != REGNO (operands[2]))))
    {
      return gen_split_359 (operands);
    }
 L2901: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1])
       || REGNO (operands[0]) != REGNO (operands[2]))))
    {
      return gen_split_360 (operands);
    }
  goto ret0;

 L2908: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2909;
  goto ret0;

 L2909: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2910;
    }
  goto ret0;

 L2910: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0))
    {
      return gen_split_361 (operands);
    }
 L2919: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0))
    {
      return gen_split_362 (operands);
    }
  goto ret0;

 L2948: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2949;
    }
  goto ret0;

 L2949: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2950;
    }
  if (small_int (x3, DImode))
    {
      operands[2] = x3;
      goto L2968;
    }
  goto ret0;

 L2950: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2951;
  goto ret0;

 L2951: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2952;
    }
  goto ret0;

 L2952: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))))
    {
      return gen_split_369 (operands);
    }
 L2961: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))))
    {
      return gen_split_370 (operands);
    }
  goto ret0;

 L2968: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2969;
  goto ret0;

 L2969: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2970;
    }
  goto ret0;

 L2970: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0))
    {
      return gen_split_371 (operands);
    }
 L2979: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0))
    {
      return gen_split_372 (operands);
    }
  goto ret0;

 L3186: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3187;
    }
  goto ret0;

 L3187: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (small_int (x3, SImode))
    {
      operands[2] = x3;
      goto L3188;
    }
  goto ret0;

 L3188: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L3189;
  goto ret0;

 L3189: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L3190;
    }
  goto ret0;

 L3190: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return gen_split_460 (operands);
    }
 L3199: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return gen_split_461 (operands);
    }
 L3208: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return gen_split_462 (operands);
    }
 L3217: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return gen_split_463 (operands);
    }
  goto ret0;

 L3234: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3235;
    }
  goto ret0;

 L3235: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (small_int (x3, SImode))
    {
      operands[2] = x3;
      goto L3236;
    }
  goto ret0;

 L3236: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L3237;
  goto ret0;

 L3237: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L3238;
    }
  goto ret0;

 L3238: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return gen_split_468 (operands);
    }
 L3247: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return gen_split_469 (operands);
    }
 L3256: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return gen_split_470 (operands);
    }
 L3265: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return gen_split_471 (operands);
    }
  goto ret0;

 L3288: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3289;
    }
  goto ret0;

 L3289: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (small_int (x3, SImode))
    {
      operands[2] = x3;
      goto L3290;
    }
  goto ret0;

 L3290: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L3291;
  goto ret0;

 L3291: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L3292;
    }
  goto ret0;

 L3292: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return gen_split_477 (operands);
    }
 L3301: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0))
    {
      return gen_split_478 (operands);
    }
 L3310: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return gen_split_479 (operands);
    }
 L3319: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0))
    {
      return gen_split_480 (operands);
    }
  goto ret0;

 L3849: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 11)
    goto L3400;
  goto ret0;

 L3400: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (register_operand (x2, VOIDmode))
    {
      operands[0] = x2;
      goto L3401;
    }
  goto ret0;

 L3401: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L3402;
  goto ret0;

 L3402: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, VOIDmode))
    {
      operands[1] = x2;
      goto L3403;
    }
  goto ret0;

 L3403: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_DEBUG_D_MODE))
    {
      return gen_split_542 (operands);
    }
  goto ret0;

 L2993: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L2994;
  goto ret0;

 L2994: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L2995;
    }
  goto ret0;

 L2995: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L3855;
  goto ret0;

 L3855: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2996;
    case MINUS:
      goto L3030;
    default:
     break;
   }
  goto ret0;

 L2996: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L2997;
  goto ret0;

 L2997: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L2998;
    }
  goto ret0;

 L2998: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L2999;
    }
  goto ret0;

 L2999: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L3000;
    }
  goto ret0;

 L3000: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L3001;
  goto ret0;

 L3001: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L3002;
    }
  goto ret0;

 L3002: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L3003;
  goto ret0;

 L3003: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L3004;
    }
  goto ret0;

 L3004: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L3005;
  goto ret0;

 L3005: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L3006;
    }
  goto ret0;

 L3006: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L3007;
  goto ret0;

 L3007: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L3008;
    }
  goto ret0;

 L3008: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && GP_REG_P (true_regnum (operands[3]))))
    {
      return gen_split_378 (operands);
    }
 L3025: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && true_regnum (operands[3]) == LO_REGNUM))
    {
      return gen_split_379 (operands);
    }
  goto ret0;

 L3030: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3031;
    }
  goto ret0;

 L3031: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L3032;
  goto ret0;

 L3032: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3033;
    }
  goto ret0;

 L3033: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L3034;
    }
  goto ret0;

 L3034: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L3035;
  goto ret0;

 L3035: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L3036;
    }
  goto ret0;

 L3036: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L3037;
  goto ret0;

 L3037: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L3038;
    }
  goto ret0;

 L3038: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L3039;
  goto ret0;

 L3039: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L3040;
    }
  goto ret0;

 L3040: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L3041;
  goto ret0;

 L3041: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L3042;
    }
  goto ret0;

 L3042: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && GP_REG_P (true_regnum (operands[1]))))
    {
      return gen_split_380 (operands);
    }
 L3059: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && true_regnum (operands[1]) == LO_REGNUM))
    {
      return gen_split_381 (operands);
    }
 L3076: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && GP_REG_P (true_regnum (operands[1]))))
    {
      return gen_split_382 (operands);
    }
  goto ret0;

 L3157: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L3158;
  goto ret0;

 L3158: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BLKmode
      && GET_CODE (x2) == MEM)
    goto L3159;
  goto ret0;

 L3159: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L3160;
    }
  goto ret0;

 L3160: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BLKmode
      && GET_CODE (x2) == MEM)
    goto L3161;
  goto ret0;

 L3161: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3162;
    }
  goto ret0;

 L3162: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L3163;
  goto ret0;

 L3163: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L3164;
    }
  goto ret0;

 L3164: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L3165;
  goto ret0;

 L3165: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[5] = x2;
      goto L3166;
    }
  goto ret0;

 L3166: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L3167;
  goto ret0;

 L3167: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[6] = x2;
      goto L3168;
    }
  goto ret0;

 L3168: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L3169;
  goto ret0;

 L3169: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      operands[7] = x2;
      goto L3170;
    }
  goto ret0;

 L3170: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == USE)
    goto L3171;
  goto ret0;

 L3171: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (small_int (x2, SImode))
    {
      operands[2] = x2;
      goto L3172;
    }
  goto ret0;

 L3172: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 6);
  if (GET_CODE (x1) == USE)
    goto L3173;
  goto ret0;

 L3173: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (small_int (x2, SImode))
    {
      operands[3] = x2;
      goto L3174;
    }
  goto ret0;

 L3174: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 7);
  if (GET_CODE (x1) == USE)
    goto L3175;
  goto ret0;

 L3175: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL
      && (reload_completed && 0 && INTVAL (operands[2]) > 0))
    {
      return gen_split_456 (operands);
    }
  goto ret0;
 ret0:
  return 0;
}

rtx split_insns PARAMS ((rtx, rtx));
rtx
split_insns (x0, insn)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x1 ATTRIBUTE_UNUSED;
  rtx x2 ATTRIBUTE_UNUSED;
  rtx x3 ATTRIBUTE_UNUSED;
  rtx x4 ATTRIBUTE_UNUSED;
  rtx x5 ATTRIBUTE_UNUSED;
  rtx x6 ATTRIBUTE_UNUSED;
  rtx x7 ATTRIBUTE_UNUSED;
  rtx tem ATTRIBUTE_UNUSED;
  recog_data.insn = NULL_RTX;

  switch (GET_CODE (x0))
    {
    case SET:
      goto L2873;
    case PARALLEL:
      goto L3806;
    default:
     break;
   }
  goto ret0;

 L2873: ATTRIBUTE_UNUSED_LABEL
  return split_1 (x0, insn);

 L3806: ATTRIBUTE_UNUSED_LABEL
  return split_2 (x0, insn);
 ret0:
  return 0;
}

