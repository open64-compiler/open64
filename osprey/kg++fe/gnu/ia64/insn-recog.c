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


extern rtx gen_split_276 PARAMS ((rtx *));
extern rtx gen_split_277 PARAMS ((rtx *));
extern rtx gen_split_281 PARAMS ((rtx *));
extern rtx gen_split_283 PARAMS ((rtx *));
extern rtx gen_split_284 PARAMS ((rtx *));
extern rtx gen_split_293 PARAMS ((rtx *));
extern rtx gen_split_294 PARAMS ((rtx *));
extern rtx gen_split_301 PARAMS ((rtx *));
extern rtx gen_split_302 PARAMS ((rtx *));
extern rtx gen_split_303 PARAMS ((rtx *));
extern rtx gen_split_304 PARAMS ((rtx *));
extern rtx gen_split_305 PARAMS ((rtx *));
extern rtx gen_split_306 PARAMS ((rtx *));
extern rtx gen_split_307 PARAMS ((rtx *));
extern rtx gen_split_308 PARAMS ((rtx *));
extern rtx gen_split_309 PARAMS ((rtx *));
extern rtx gen_split_310 PARAMS ((rtx *));
extern rtx gen_split_311 PARAMS ((rtx *));
extern rtx gen_split_312 PARAMS ((rtx *));
extern rtx gen_peephole2_313 PARAMS ((rtx, rtx *));
extern rtx gen_split_323 PARAMS ((rtx *));
extern rtx gen_split_324 PARAMS ((rtx *));
extern rtx gen_split_335 PARAMS ((rtx *));
extern rtx gen_split_336 PARAMS ((rtx *));
extern rtx gen_split_338 PARAMS ((rtx *));
extern rtx gen_split_339 PARAMS ((rtx *));
extern rtx gen_split_341 PARAMS ((rtx *));
extern rtx gen_split_342 PARAMS ((rtx *));
extern rtx gen_split_344 PARAMS ((rtx *));
extern rtx gen_split_345 PARAMS ((rtx *));
extern rtx gen_split_350 PARAMS ((rtx *));
extern rtx gen_split_352 PARAMS ((rtx *));
extern rtx gen_split_353 PARAMS ((rtx *));
extern rtx gen_split_387 PARAMS ((rtx *));
extern rtx gen_split_388 PARAMS ((rtx *));
extern rtx gen_split_389 PARAMS ((rtx *));
extern rtx gen_split_390 PARAMS ((rtx *));
extern rtx gen_split_391 PARAMS ((rtx *));
extern rtx gen_split_392 PARAMS ((rtx *));
extern rtx gen_split_393 PARAMS ((rtx *));
extern rtx gen_split_394 PARAMS ((rtx *));
extern rtx gen_split_395 PARAMS ((rtx *));
extern rtx gen_split_401 PARAMS ((rtx *));
extern rtx gen_split_402 PARAMS ((rtx *));
extern rtx gen_split_403 PARAMS ((rtx *));
extern rtx gen_split_404 PARAMS ((rtx *));
extern rtx gen_split_405 PARAMS ((rtx *));
extern rtx gen_split_416 PARAMS ((rtx *));



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

  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case AND:
      goto L338;
    case IOR:
      goto L349;
    case NOT:
      goto L367;
    case EQ:
      goto L1508;
    case NE:
      goto L1516;
    default:
     break;
   }
 L4891: ATTRIBUTE_UNUSED_LABEL
  if (normal_comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L1472;
    }
  if (adjusted_comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L1477;
    }
 L4893: ATTRIBUTE_UNUSED_LABEL
  if (comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L1492;
    }
  goto ret0;

 L338: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L4897;
  goto ret0;

 L4897: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case NOT:
      goto L344;
    case NE:
      goto L433;
    case EQ:
      goto L442;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L4896;
    default:
      goto L4898;
   }
 L4896: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L339;
    }
 L4898: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x2, BImode))
    {
      operands[4] = x2;
      goto L403;
    }
  if (signed_inequality_operator (x2, BImode))
    {
      operands[3] = x2;
      goto L380;
    }
  goto ret0;

 L344: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L4902;
  goto ret0;

 L4902: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L345;
    }
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L418;
    }
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L396;
    }
  goto ret0;

 L345: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 64;
    }
  goto ret0;

 L418: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L419;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L389;
    }
  goto ret0;

 L419: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L420;
    }
  goto ret0;

 L420: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 74;
    }
  goto ret0;

 L389: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L390;
    }
  goto ret0;

 L390: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 70;
    }
  goto ret0;

 L396: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L4905;
    case DImode:
      goto L4906;
    default:
      break;
    }
  goto ret0;

 L4905: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L397;
    }
  goto ret0;

 L397: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L398;
  goto ret0;

 L398: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 71;
    }
  goto ret0;

 L4906: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L427;
    }
  goto ret0;

 L427: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L428;
  goto ret0;

 L428: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 75;
    }
  goto ret0;

 L433: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4907;
  goto L4898;

 L4907: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L434;
    case ZERO_EXTRACT:
      goto L452;
    default:
     break;
   }
  goto L4898;

 L434: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L435;
    }
  goto L4898;

 L435: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L436;
  goto L4898;

 L436: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L437;
  goto L4898;

 L437: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 76;
    }
  x2 = XEXP (x1, 0);
  goto L4898;

 L452: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L453;
    }
  goto L4898;

 L453: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L454;
  goto L4898;

 L454: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L455;
    }
  goto L4898;

 L455: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L456;
  goto L4898;

 L456: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 78;
    }
  x2 = XEXP (x1, 0);
  goto L4898;

 L442: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4909;
  goto L4898;

 L4909: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L443;
    case ZERO_EXTRACT:
      goto L462;
    default:
     break;
   }
  goto L4898;

 L443: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L444;
    }
  goto L4898;

 L444: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L445;
  goto L4898;

 L445: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L446;
  goto L4898;

 L446: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 77;
    }
  x2 = XEXP (x1, 0);
  goto L4898;

 L462: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L463;
    }
  goto L4898;

 L463: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L464;
  goto L4898;

 L464: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L465;
    }
  goto L4898;

 L465: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L466;
  goto L4898;

 L466: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 79;
    }
  x2 = XEXP (x1, 0);
  goto L4898;

 L339: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 63;
    }
  goto ret0;

 L403: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L404;
    }
  if (gr_reg_or_0_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L374;
    }
  goto ret0;

 L404: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L405;
    }
  goto ret0;

 L405: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 72;
    }
  goto ret0;

 L374: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L375;
    }
  goto ret0;

 L375: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 68;
    }
  goto ret0;

 L380: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L4911;
    case DImode:
      goto L4912;
    default:
      break;
    }
  goto ret0;

 L4911: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L381;
    }
  goto ret0;

 L381: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L382;
  goto ret0;

 L382: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 69;
    }
  goto ret0;

 L4912: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L411;
    }
  goto ret0;

 L411: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L412;
  goto ret0;

 L412: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 73;
    }
  goto ret0;

 L349: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L4914;
  goto ret0;

 L4914: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case NOT:
      goto L355;
    case NE:
      goto L531;
    case EQ:
      goto L540;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L4913;
    default:
      goto L4915;
   }
 L4913: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L350;
    }
 L4915: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x2, BImode))
    {
      operands[4] = x2;
      goto L501;
    }
  if (signed_inequality_operator (x2, BImode))
    {
      operands[3] = x2;
      goto L478;
    }
  goto ret0;

 L355: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L4919;
  goto ret0;

 L4919: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L356;
    }
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L516;
    }
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L494;
    }
  goto ret0;

 L356: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 66;
    }
  goto ret0;

 L516: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L517;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L487;
    }
  goto ret0;

 L517: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L518;
    }
  goto ret0;

 L518: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 86;
    }
  goto ret0;

 L487: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L488;
    }
  goto ret0;

 L488: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 82;
    }
  goto ret0;

 L494: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L4922;
    case DImode:
      goto L4923;
    default:
      break;
    }
  goto ret0;

 L4922: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L495;
    }
  goto ret0;

 L495: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L496;
  goto ret0;

 L496: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 83;
    }
  goto ret0;

 L4923: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L525;
    }
  goto ret0;

 L525: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L526;
  goto ret0;

 L526: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 87;
    }
  goto ret0;

 L531: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4924;
  goto L4915;

 L4924: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L532;
    case ZERO_EXTRACT:
      goto L550;
    default:
     break;
   }
  goto L4915;

 L532: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L533;
    }
  goto L4915;

 L533: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L534;
  goto L4915;

 L534: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L535;
  goto L4915;

 L535: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 88;
    }
  x2 = XEXP (x1, 0);
  goto L4915;

 L550: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L551;
    }
  goto L4915;

 L551: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L552;
  goto L4915;

 L552: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L553;
    }
  goto L4915;

 L553: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L554;
  goto L4915;

 L554: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 90;
    }
  x2 = XEXP (x1, 0);
  goto L4915;

 L540: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4926;
  goto L4915;

 L4926: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L541;
    case ZERO_EXTRACT:
      goto L560;
    default:
     break;
   }
  goto L4915;

 L541: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L542;
    }
  goto L4915;

 L542: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L543;
  goto L4915;

 L543: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L544;
  goto L4915;

 L544: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 89;
    }
  x2 = XEXP (x1, 0);
  goto L4915;

 L560: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L561;
    }
  goto L4915;

 L561: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L562;
  goto L4915;

 L562: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L563;
    }
  goto L4915;

 L563: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L564;
  goto L4915;

 L564: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 91;
    }
  x2 = XEXP (x1, 0);
  goto L4915;

 L350: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 65;
    }
  goto ret0;

 L501: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L502;
    }
  if (gr_reg_or_0_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L472;
    }
  goto ret0;

 L502: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L503;
    }
  goto ret0;

 L503: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 84;
    }
  goto ret0;

 L472: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L473;
    }
  goto ret0;

 L473: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 80;
    }
  goto ret0;

 L478: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L4928;
    case DImode:
      goto L4929;
    default:
      break;
    }
  goto ret0;

 L4928: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L479;
    }
  goto ret0;

 L479: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L480;
  goto ret0;

 L480: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 81;
    }
  goto ret0;

 L4929: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L509;
    }
  goto ret0;

 L509: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L510;
  goto ret0;

 L510: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 85;
    }
  goto ret0;

 L367: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L368;
    }
  goto ret0;

 L368: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 67;
    }
  goto ret0;

 L1508: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ZERO_EXTRACT)
    goto L1509;
  goto L4891;

 L1509: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1510;
    }
  goto L4891;

 L1510: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    goto L1511;
  goto L4891;

 L1511: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (immediate_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1512;
    }
  goto L4891;

 L1512: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return 212;
    }
  goto L4891;

 L1516: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ZERO_EXTRACT)
    goto L1517;
  goto L4891;

 L1517: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1518;
    }
  goto L4891;

 L1518: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    goto L1519;
  goto L4891;

 L1519: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (immediate_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1520;
    }
  goto L4891;

 L1520: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return 213;
    }
  goto L4891;

 L1472: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1473;
    }
  if (gr_reg_or_0_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1483;
    }
  goto L4893;

 L1473: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_operand (x2, SImode))
    {
      operands[3] = x2;
      return 205;
    }
  goto L4893;

 L1483: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_operand (x2, DImode))
    {
      operands[3] = x2;
      return 207;
    }
  goto L4893;

 L1477: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L4930;
    case DImode:
      goto L4931;
    default:
      break;
    }
  goto L4893;

 L4930: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1478;
    }
  goto L4893;

 L1478: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_adjusted_operand (x2, SImode))
    {
      operands[3] = x2;
      return 206;
    }
  goto L4893;

 L4931: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1488;
    }
  goto L4893;

 L1488: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_adjusted_operand (x2, DImode))
    {
      operands[3] = x2;
      return 208;
    }
  goto L4893;

 L1492: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L4932;
    case DFmode:
      goto L4933;
    case TFmode:
      goto L4934;
    default:
      break;
    }
  goto ret0;

 L4932: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L1493;
    }
  goto ret0;

 L1493: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 209;
    }
  goto ret0;

 L4933: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L1498;
    }
  goto ret0;

 L1498: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 210;
    }
  goto ret0;

 L4934: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1503;
    }
  goto ret0;

 L1503: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1504;
    }
  goto ret0;

 L1504: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 211;
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
  if (GET_CODE (x1) == ZERO_EXTRACT)
    goto L290;
  if (destination_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1571;
    }
 L4865: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L40;
    }
 L4871: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L156;
    }
 L4872: ATTRIBUTE_UNUSED_LABEL
  if (grfr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L164;
    }
 L4878: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L223;
    }
  goto ret0;

 L290: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L291;
    }
  goto ret0;

 L291: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT)
    goto L4954;
  goto ret0;

 L4954: ATTRIBUTE_UNUSED_LABEL
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L292;
    }
 L4955: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x2, 0) == 32L)
    goto L316;
  goto ret0;

 L292: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L293;
    }
  x2 = XEXP (x1, 1);
  goto L4955;

 L293: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, DImode))
    {
      operands[3] = x1;
      goto L294;
    }
  x1 = XEXP (x0, 0);
  x2 = XEXP (x1, 1);
  goto L4955;

 L294: ATTRIBUTE_UNUSED_LABEL
  if (((gr_register_operand (operands[3], DImode) && INTVAL (operands[1]) <= 16)
   || operands[3] == const0_rtx || operands[3] == constm1_rtx))
    {
      return 57;
    }
  x1 = XEXP (x0, 0);
  x2 = XEXP (x1, 1);
  goto L4955;

 L316: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == CONST_INT)
    goto L4956;
  goto ret0;

 L4956: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x2, 0) == XWINT (x2, 0))
    switch ((int) XWINT (x2, 0))
      {
      case 0L:
        goto L317;
      case 32L:
        goto L325;
      default:
        break;
      }
  goto ret0;

 L317: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == LSHIFTRT)
    goto L318;
  goto ret0;

 L318: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L319;
    }
  goto ret0;

 L319: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 32L)
    {
      return 60;
    }
  goto ret0;

 L325: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (gr_reg_or_0_operand (x1, DImode))
    {
      operands[1] = x1;
      return 61;
    }
  goto ret0;

 L1571: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == IF_THEN_ELSE)
    goto L1572;
  if (move_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L37;
    }
  x1 = XEXP (x0, 0);
  goto L4865;

 L1572: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1573;
    }
  x1 = XEXP (x0, 0);
  goto L4865;

 L1573: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1574;
    }
  x1 = XEXP (x0, 0);
  goto L4865;

 L1574: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L1575;
  x1 = XEXP (x0, 0);
  goto L4865;

 L1575: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1576;
    }
  x1 = XEXP (x0, 0);
  goto L4865;

 L1576: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (move_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1577;
    }
  x1 = XEXP (x0, 0);
  goto L4865;

 L1577: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[2])
   && ia64_move_ok (operands[0], operands[3])))
    {
      return 219;
    }
  x1 = XEXP (x0, 0);
  goto L4865;

 L37: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 7;
    }
  x1 = XEXP (x0, 0);
  goto L4865;

 L40: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L4958;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4958: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L41;
    case MINUS:
      goto L51;
    case LO_SUM:
      goto L62;
    case UNSPEC:
      goto L4964;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4871;

 L41: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L4967;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4967: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case REG:
      goto L4971;
    case HIGH:
      goto L57;
    case PLUS:
      goto L709;
    default:
     break;
   }
 L4969: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L89;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L4971: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 0) == 1)
    goto L68;
  goto L4969;

 L68: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L4972;
 L42: ATTRIBUTE_UNUSED_LABEL
  if (function_operand (x2, VOIDmode))
    {
      operands[1] = x2;
      return 8;
    }
 L47: ATTRIBUTE_UNUSED_LABEL
  if (sdata_symbolic_operand (x2, VOIDmode))
    {
      operands[1] = x2;
      return 9;
    }
  x2 = XEXP (x1, 0);
  goto L4969;

 L4972: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L4975;
  goto L42;

 L4975: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1)
    goto L4978;
  goto L42;

 L4978: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x2, 1))
    {
    case 0L:
      goto L69;
    case 1L:
      goto L75;
    case 3L:
      goto L104;
    default:
      break;
    }
  goto L42;

 L69: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      return 13;
    }
  goto L42;

 L75: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      return 14;
    }
  goto L42;

 L104: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      return 19;
    }
  goto L42;

 L57: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (got_symbolic_operand (x3, VOIDmode))
    {
      operands[1] = x3;
      goto L58;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L58: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 11;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L709: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L710;
  x1 = XEXP (x0, 0);
  goto L4871;

 L710: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L711;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L711: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L712;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L712: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L713;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L713: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L714;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L714: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 110;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L89: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L4981;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4981: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L4983;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4983: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1)
    goto L4985;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4985: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x2, 1))
    {
    case 2L:
      goto L90;
    case 4L:
      goto L119;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L90: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L91;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L91: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS14))
    {
      return 17;
    }
 L98: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS22))
    {
      return 18;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L119: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L120;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L120: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS14))
    {
      return 22;
    }
 L127: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS22))
    {
      return 23;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L51: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L52;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L52: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1)
    {
      return 10;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L62: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L63;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L63: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (got_symbolic_operand (x2, VOIDmode))
    {
      operands[2] = x2;
      return 12;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L4964: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1)
    goto L4987;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4987: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x1, 1))
    {
    case 2L:
      goto L79;
    case 4L:
      goto L108;
    case 21L:
      goto L1793;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L79: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_MODE (x2) == DImode)
    goto L4990;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4990: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L80;
    }
 L4991: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      return 16;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L80: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 15;
    }
  x1 = XEXP (x0, 1);
  x2 = XVECEXP (x1, 0, 0);
  goto L4991;

 L108: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_MODE (x2) == DImode)
    goto L4992;
  x1 = XEXP (x0, 0);
  goto L4871;

 L4992: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L109;
    }
 L4993: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      return 21;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L109: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 20;
    }
  x1 = XEXP (x0, 1);
  x2 = XVECEXP (x1, 0, 0);
  goto L4993;

 L1793: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return 246;
    }
  x1 = XEXP (x0, 0);
  goto L4871;

 L156: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L4994;
  x1 = XEXP (x0, 0);
  goto L4872;

 L4994: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case SIGN_EXTEND:
      goto L157;
    case ZERO_EXTEND:
      goto L169;
    case SIGN_EXTRACT:
      goto L279;
    case ZERO_EXTRACT:
      goto L285;
    case AND:
      goto L298;
    case IOR:
      goto L329;
    case PLUS:
      goto L642;
    case MINUS:
      goto L661;
    case NEG:
      goto L738;
    case UNSPEC:
      goto L5013;
    case ASHIFT:
      goto L1401;
    case ASHIFTRT:
      goto L1423;
    case LSHIFTRT:
      goto L1428;
    case ROTATERT:
      goto L1433;
    case ROTATE:
      goto L1438;
    case NOT:
      goto L1468;
    case NE:
      goto L1562;
    case EQ:
      goto L1567;
    case IF_THEN_ELSE:
      goto L1581;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4872;

 L157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case QImode:
      goto L5014;
    case HImode:
      goto L5015;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L5014: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, QImode))
    {
      operands[1] = x2;
      return 29;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L5015: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, HImode))
    {
      operands[1] = x2;
      return 30;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L169: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case QImode:
      goto L5016;
    case HImode:
      goto L5017;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L5016: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x2, QImode))
    {
      operands[1] = x2;
      return 32;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L5017: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x2, HImode))
    {
      operands[1] = x2;
      return 33;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L279: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L280;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L280: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L281;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L281: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      return 55;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L285: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L286;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L286: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L287;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L287: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      return 56;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L298: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ASHIFT)
    goto L299;
  x1 = XEXP (x0, 0);
  goto L4872;

 L299: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L300;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L300: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L301;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L301: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L302;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L302: ATTRIBUTE_UNUSED_LABEL
  if ((CONST_OK_FOR_M (INTVAL (operands[2]))
   && ia64_depz_field_mask (operands[3], operands[2]) > 0))
    {
      return 58;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L329: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ZERO_EXTEND)
    goto L330;
  x1 = XEXP (x0, 0);
  goto L4872;

 L330: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L331;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L331: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ASHIFT)
    goto L332;
  x1 = XEXP (x0, 0);
  goto L4872;

 L332: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTEND)
    goto L333;
  x1 = XEXP (x0, 0);
  goto L4872;

 L333: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L334;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L334: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32L)
    {
      return 62;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L642: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L5019;
  x1 = XEXP (x0, 0);
  goto L4872;

 L5019: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L648;
    case MULT:
      goto L655;
    case NOT:
      goto L667;
    case SUBREG:
    case REG:
      goto L5018;
    default:
      x1 = XEXP (x0, 0);
      goto L4872;
   }
 L5018: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L643;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L648: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L5023;
  x1 = XEXP (x0, 0);
  goto L4872;

 L5023: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L1415;
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L649;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1415: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L1416;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1416: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L1417;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1417: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L1418;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1418: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L1419;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1419: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 194;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L649: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L650;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L650: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    {
      return 104;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L655: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L656;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L656: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT)
    goto L5024;
  x1 = XEXP (x0, 0);
  goto L4872;

 L5024: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x3, 0) == 2L)
    goto L657;
 L5025: ATTRIBUTE_UNUSED_LABEL
  if (shladd_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1409;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L657: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    {
      return 105;
    }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 1);
  goto L5025;

 L1409: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[3] = x2;
      return 193;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L667: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L668;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L668: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 107;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L643: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_22bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 103;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L661: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_reg_or_8bit_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L662;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L662: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 106;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L738: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 113;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L5013: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 16)
    goto L742;
  x1 = XEXP (x0, 0);
  goto L4872;

 L742: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 114;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1401: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1402;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1402: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_6bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 192;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1423: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1424;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1424: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_6bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 195;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1428: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1429;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1429: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_6bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 196;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1433: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1434;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1434: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_count_operand (x2, DImode))
    {
      operands[2] = x2;
      return 197;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1438: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1439;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1439: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_count_operand (x2, DImode))
    {
      operands[2] = x2;
      return 198;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1468: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 204;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1562: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1563;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1563: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return 217;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1567: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1568;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1568: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return 218;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1581: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1582;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1582: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1583;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1583: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L1584;
  x1 = XEXP (x0, 0);
  goto L4872;

 L1584: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == NEG)
    goto L1585;
  x1 = XEXP (x0, 0);
  goto L4872;

 L1585: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1586;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L1586: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, DImode))
    {
      operands[3] = x2;
      return 220;
    }
  x1 = XEXP (x0, 0);
  goto L4872;

 L164: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L5026;
  x1 = XEXP (x0, 0);
  goto L4878;

 L5026: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case SIGN_EXTEND:
      goto L165;
    case ZERO_EXTEND:
      goto L177;
    case AND:
      goto L1447;
    case IOR:
      goto L1458;
    case XOR:
      goto L1463;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4878;

 L165: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 31;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L177: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_nonimmediate_operand (x2, SImode))
    {
      operands[1] = x2;
      return 34;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1447: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L5032;
  x1 = XEXP (x0, 0);
  goto L4878;

 L5032: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L1453;
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1448;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1453: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1454;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1454: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 201;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1448: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 200;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1458: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1459;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1459: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 202;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1463: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1464;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L1464: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 203;
    }
  x1 = XEXP (x0, 0);
  goto L4878;

 L223: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L5033;
  goto ret0;

 L5033: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case FIX:
      goto L224;
    case UNSIGNED_FIX:
      goto L258;
    case MULT:
      goto L672;
    case PLUS:
      goto L687;
    case TRUNCATE:
      goto L718;
    default:
     break;
   }
  goto ret0;

 L224: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L5038;
    case DFmode:
      goto L5039;
    case TFmode:
      goto L5040;
    default:
      break;
    }
  goto ret0;

 L5038: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 44;
    }
  goto ret0;

 L5039: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 45;
    }
  goto ret0;

 L5040: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L233;
    }
  goto ret0;

 L233: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 46;
    }
  goto ret0;

 L258: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L5041;
    case DFmode:
      goto L5042;
    case TFmode:
      goto L5043;
    default:
      break;
    }
  goto ret0;

 L5041: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 51;
    }
  goto ret0;

 L5042: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 52;
    }
  goto ret0;

 L5043: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L267;
    }
  goto ret0;

 L267: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 53;
    }
  goto ret0;

 L672: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L673;
    }
  goto ret0;

 L673: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 108;
    }
  goto ret0;

 L687: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MULT)
    goto L688;
  goto ret0;

 L688: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L689;
    }
  goto ret0;

 L689: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L690;
    }
  goto ret0;

 L690: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L691;
    }
  goto ret0;

 L691: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 109;
    }
  goto ret0;

 L718: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TImode
      && GET_CODE (x2) == LSHIFTRT)
    goto L719;
  goto ret0;

 L719: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode
      && GET_CODE (x3) == MULT)
    goto L720;
  goto ret0;

 L720: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TImode)
    goto L5044;
  goto ret0;

 L5044: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case SIGN_EXTEND:
      goto L721;
    case ZERO_EXTEND:
      goto L731;
    default:
     break;
   }
  goto ret0;

 L721: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L722;
    }
  goto ret0;

 L722: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == SIGN_EXTEND)
    goto L723;
  goto ret0;

 L723: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L724;
    }
  goto ret0;

 L724: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 64L)
    {
      return 111;
    }
  goto ret0;

 L731: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L732;
    }
  goto ret0;

 L732: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == ZERO_EXTEND)
    goto L733;
  goto ret0;

 L733: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L734;
    }
  goto ret0;

 L734: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 64L)
    {
      return 112;
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
    case FLOAT_TRUNCATE:
      goto L195;
    case UNSIGNED_FLOAT:
      goto L245;
    case PLUS:
      goto L790;
    case MINUS:
      goto L795;
    case MULT:
      goto L800;
    case ABS:
      goto L805;
    case NEG:
      goto L809;
    case SMIN:
      goto L818;
    case SMAX:
      goto L823;
    case DIV:
      goto L869;
    default:
     break;
   }
  goto ret0;

 L195: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L5058;
    case TFmode:
      goto L5062;
    default:
      break;
    }
  goto ret0;

 L5058: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L900;
    case MINUS:
      goto L911;
    case MULT:
      goto L922;
    case NEG:
      goto L987;
    case SUBREG:
    case REG:
      goto L5056;
    default:
      goto ret0;
   }
 L5056: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 38;
    }
  goto ret0;

 L900: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L5067;
  goto ret0;

 L5067: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L959;
    case NEG:
      goto L1014;
    case SUBREG:
    case REG:
      goto L5066;
    default:
      goto ret0;
   }
 L5066: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L901;
    }
  goto ret0;

 L959: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L960;
    }
  goto ret0;

 L960: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L961;
    }
  goto ret0;

 L961: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 143;
    }
  goto ret0;

 L1014: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L1015;
  goto ret0;

 L1015: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L1016;
    }
  goto ret0;

 L1016: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L1017;
    }
  goto ret0;

 L1017: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 150;
    }
  goto ret0;

 L901: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 132;
    }
  goto ret0;

 L911: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L5070;
  goto ret0;

 L5070: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L974;
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L912;
    }
  goto ret0;

 L974: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L975;
    }
  goto ret0;

 L975: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L976;
    }
  goto ret0;

 L976: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 145;
    }
  goto ret0;

 L912: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 134;
    }
  goto ret0;

 L922: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L923;
    }
  goto ret0;

 L923: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 136;
    }
  goto ret0;

 L987: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L988;
  goto ret0;

 L988: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L989;
    }
  goto ret0;

 L989: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 147;
    }
  goto ret0;

 L5062: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L1068;
    case MINUS:
      goto L1088;
    case MULT:
      goto L1108;
    case NEG:
      goto L1261;
    case SUBREG:
    case REG:
      goto L5057;
    default:
      goto ret0;
   }
 L5057: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L200;
    }
  goto ret0;

 L1068: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5072;
  goto ret0;

 L5072: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1188;
    case NEG:
      goto L1287;
    case REG:
    case CONST_DOUBLE:
      goto L5071;
    default:
      goto ret0;
   }
 L5071: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1069;
    }
  goto ret0;

 L1188: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1189;
    }
  goto ret0;

 L1189: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1190;
    }
  goto ret0;

 L1190: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1191;
    }
  goto ret0;

 L1191: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 171;
    }
  goto ret0;

 L1287: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L1288;
  goto ret0;

 L1288: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1289;
    }
  goto ret0;

 L1289: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1290;
    }
  goto ret0;

 L1290: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1291;
    }
  goto ret0;

 L1291: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 182;
    }
  goto ret0;

 L1069: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1070;
    }
  goto ret0;

 L1070: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 154;
    }
  goto ret0;

 L1088: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5075;
  goto ret0;

 L5075: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L1237;
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1089;
    }
  goto ret0;

 L1237: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1238;
    }
  goto ret0;

 L1238: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1239;
    }
  goto ret0;

 L1239: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1240;
    }
  goto ret0;

 L1240: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 176;
    }
  goto ret0;

 L1089: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1090;
    }
  goto ret0;

 L1090: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 157;
    }
  goto ret0;

 L1108: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1109;
    }
  goto ret0;

 L1109: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1110;
    }
  goto ret0;

 L1110: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 160;
    }
  goto ret0;

 L1261: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1262;
  goto ret0;

 L1262: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1263;
    }
  goto ret0;

 L1263: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1264;
    }
  goto ret0;

 L1264: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 179;
    }
  goto ret0;

 L200: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 39;
    }
  goto ret0;

 L245: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 48;
    }
  goto ret0;

 L790: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L5077;
  goto ret0;

 L5077: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L829;
    case NEG:
      goto L849;
    case SUBREG:
    case REG:
      goto L5076;
    default:
      goto ret0;
   }
 L5076: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L791;
    }
  goto ret0;

 L829: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L830;
    }
  goto ret0;

 L830: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L831;
    }
  goto ret0;

 L831: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 125;
    }
  goto ret0;

 L849: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == MULT)
    goto L850;
  goto ret0;

 L850: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L851;
    }
  goto ret0;

 L851: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      goto L852;
    }
  goto ret0;

 L852: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 128;
    }
  goto ret0;

 L791: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 117;
    }
  goto ret0;

 L795: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L5080;
  goto ret0;

 L5080: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L836;
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L796;
    }
  goto ret0;

 L836: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L837;
    }
  goto ret0;

 L837: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L838;
    }
  goto ret0;

 L838: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 126;
    }
  goto ret0;

 L796: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 118;
    }
  goto ret0;

 L800: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L801;
    }
  goto ret0;

 L801: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 119;
    }
  goto ret0;

 L805: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 120;
    }
  goto ret0;

 L809: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L5082;
  goto ret0;

 L5082: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L814;
    case MULT:
      goto L843;
    case SUBREG:
    case REG:
      goto L5081;
    default:
      goto ret0;
   }
 L5081: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 121;
    }
  goto ret0;

 L814: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 122;
    }
  goto ret0;

 L843: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L844;
    }
  goto ret0;

 L844: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 127;
    }
  goto ret0;

 L818: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L819;
    }
  goto ret0;

 L819: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 123;
    }
  goto ret0;

 L823: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L824;
    }
  goto ret0;

 L824: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 124;
    }
  goto ret0;

 L869: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L870;
    }
  goto ret0;

 L870: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L871;
    }
  goto ret0;

 L871: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 129;
    }
 L890: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 130;
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

  x1 = XEXP (x0, 0);
  switch (GET_MODE (x1))
    {
    case CCImode:
      goto L4859;
    case BImode:
      goto L4860;
    case QImode:
      goto L4861;
    case HImode:
      goto L4862;
    case SImode:
      goto L4863;
    case DImode:
      goto L4879;
    case TImode:
      goto L4866;
    case SFmode:
      goto L4868;
    case DFmode:
      goto L4869;
    case TFmode:
      goto L4870;
    default:
      break;
    }
 L1522: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == PC)
    goto L1727;
 L1827: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x1;
  goto L1828;
 L1883: ATTRIBUTE_UNUSED_LABEL
  switch (GET_MODE (x1))
    {
    case BImode:
      goto L4886;
    case DImode:
      goto L4887;
    default:
      break;
    }
  goto ret0;

 L4859: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, CCImode))
    {
      operands[0] = x1;
      goto L2;
    }
  goto L1522;

 L2: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, CCImode))
    {
      operands[1] = x1;
      return 0;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L4860: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L5;
    }
 L4880: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L337;
    }
  goto L1522;

 L5: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, BImode))
    {
      operands[1] = x1;
      return 1;
    }
  x1 = XEXP (x0, 0);
  goto L4880;

 L337: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BImode)
    goto L4888;
  x1 = XEXP (x0, 0);
  goto L1522;

 L4888: ATTRIBUTE_UNUSED_LABEL
  tem = recog_1 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L1522;

 L4861: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, QImode))
    {
      operands[0] = x1;
      goto L8;
    }
  goto L1522;

 L8: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, QImode))
    {
      operands[1] = x1;
      goto L9;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L9: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 2;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L4862: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L12;
    }
 L4881: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L567;
    }
  goto L1522;

 L12: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, HImode))
    {
      operands[1] = x1;
      goto L13;
    }
  x1 = XEXP (x0, 0);
  goto L4881;

 L13: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 3;
    }
  x1 = XEXP (x0, 0);
  goto L4881;

 L567: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == HImode
      && GET_CODE (x1) == MULT)
    goto L568;
  x1 = XEXP (x0, 0);
  goto L1522;

 L568: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L569;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L569: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, HImode))
    {
      operands[2] = x2;
      return 92;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L4863: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L1589;
    }
 L4882: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L572;
    }
 L4883: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L609;
    }
  goto L1522;

 L1589: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == IF_THEN_ELSE)
    goto L1590;
  if (move_operand (x1, SImode))
    {
      operands[1] = x1;
      goto L25;
    }
  x1 = XEXP (x0, 0);
  goto L4882;

 L1590: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1591;
    }
  x1 = XEXP (x0, 0);
  goto L4882;

 L1591: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1592;
    }
  x1 = XEXP (x0, 0);
  goto L4882;

 L1592: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L1593;
  x1 = XEXP (x0, 0);
  goto L4882;

 L1593: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1594;
    }
  x1 = XEXP (x0, 0);
  goto L4882;

 L1594: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (move_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1595;
    }
  x1 = XEXP (x0, 0);
  goto L4882;

 L1595: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[2])
   && ia64_move_ok (operands[0], operands[3])))
    {
      return 221;
    }
  x1 = XEXP (x0, 0);
  goto L4882;

 L25: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 5;
    }
  x1 = XEXP (x0, 0);
  goto L4882;

 L572: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L4935;
  x1 = XEXP (x0, 0);
  goto L4883;

 L4935: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L573;
    case MINUS:
      goto L599;
    case NEG:
      goto L622;
    case ASHIFT:
      goto L1386;
    case ROTATERT:
      goto L1391;
    case ROTATE:
      goto L1396;
    case NOT:
      goto L1443;
    case IF_THEN_ELSE:
      goto L1599;
    default:
     break;
   }
 L4943: ATTRIBUTE_UNUSED_LABEL
  if (condop_operator (x1, SImode))
    {
      operands[5] = x1;
      goto L1608;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L573: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L4945;
  goto L4943;

 L4945: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L579;
    case MULT:
      goto L586;
    case NOT:
      goto L605;
    case SUBREG:
    case REG:
      goto L4944;
    default:
      goto L4943;
   }
 L4944: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L574;
    }
  goto L4943;

 L579: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L580;
    }
  goto L4943;

 L580: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L581;
    }
  goto L4943;

 L581: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    {
      return 94;
    }
  goto L4943;

 L586: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L587;
    }
  goto L4943;

 L587: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT)
    goto L4948;
  goto L4943;

 L4948: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x3, 0) == 2L)
    goto L588;
 L4949: ATTRIBUTE_UNUSED_LABEL
  if (shladd_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L595;
    }
  goto L4943;

 L588: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    {
      return 95;
    }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 1);
  goto L4949;

 L595: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[3] = x2;
      return 96;
    }
  goto L4943;

 L605: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L606;
    }
  goto L4943;

 L606: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 98;
    }
  goto L4943;

 L574: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_22bit_operand (x2, SImode))
    {
      operands[2] = x2;
      return 93;
    }
  goto L4943;

 L599: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_reg_or_8bit_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L600;
    }
  goto L4943;

 L600: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 97;
    }
  goto L4943;

 L622: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 101;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1386: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1387;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1387: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_5bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 189;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1391: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1392;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1392: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_5bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 190;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1396: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1397;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1397: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_32bit_count_operand (x2, SImode))
    {
      operands[2] = x2;
      return 191;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1443: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 199;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1599: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1600;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1600: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1601;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1601: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L1602;
  x1 = XEXP (x0, 0);
  goto L4883;

 L1602: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == NEG)
    goto L1603;
  x1 = XEXP (x0, 0);
  goto L4883;

 L1603: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L1604;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1604: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, SImode))
    {
      operands[2] = x2;
      return 222;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1608: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L4950;
  x1 = XEXP (x0, 0);
  goto L4883;

 L4950: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == IF_THEN_ELSE)
    goto L1609;
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1619;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1609: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L1610;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1610: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L1611;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1611: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L1612;
  x1 = XEXP (x0, 0);
  goto L4883;

 L1612: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1613;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1613: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L1614;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1614: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      return 223;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1619: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == IF_THEN_ELSE)
    goto L1620;
  x1 = XEXP (x0, 0);
  goto L4883;

 L1620: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L1621;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1621: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L1622;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1622: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L1623;
  x1 = XEXP (x0, 0);
  goto L4883;

 L1623: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1624;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L1624: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      return 224;
    }
  x1 = XEXP (x0, 0);
  goto L4883;

 L609: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L4952;
  x1 = XEXP (x0, 0);
  goto L1522;

 L4952: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case MULT:
      goto L610;
    case PLUS:
      goto L615;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1522;

 L610: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L611;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L611: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 99;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L615: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MULT)
    goto L616;
  x1 = XEXP (x0, 0);
  goto L1522;

 L616: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L617;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L617: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L618;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L618: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, SImode))
    {
      operands[3] = x2;
      return 100;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L4879: ATTRIBUTE_UNUSED_LABEL
  tem = recog_2 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  goto L1522;

 L4866: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, TImode))
    {
      operands[0] = x1;
      goto L137;
    }
 L4867: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, TImode))
    {
      operands[0] = x1;
      goto L141;
    }
  goto L1522;

 L137: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, TImode))
    {
      operands[1] = x1;
      goto L138;
    }
  x1 = XEXP (x0, 0);
  goto L4867;

 L138: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 24;
    }
  x1 = XEXP (x0, 0);
  goto L4867;

 L141: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, TImode))
    {
      operands[1] = x1;
      return 25;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L4868: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L144;
    }
 L4875: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L194;
    }
 L4877: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L218;
    }
  goto L1522;

 L144: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, SFmode))
    {
      operands[1] = x1;
      goto L145;
    }
  x1 = XEXP (x0, 0);
  goto L4875;

 L145: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 26;
    }
  x1 = XEXP (x0, 0);
  goto L4875;

 L194: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SFmode)
    goto L5046;
  x1 = XEXP (x0, 0);
  goto L4877;

 L5046: ATTRIBUTE_UNUSED_LABEL
  tem = recog_3 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L4877;

 L218: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SFmode
      && GET_CODE (x1) == FLOAT)
    goto L219;
  x1 = XEXP (x0, 0);
  goto L1522;

 L219: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L220;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L220: ATTRIBUTE_UNUSED_LABEL
  if ((!INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 43;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L4869: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L148;
    }
 L4873: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L180;
    }
 L4876: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L213;
    }
  goto L1522;

 L148: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, DFmode))
    {
      operands[1] = x1;
      goto L149;
    }
  x1 = XEXP (x0, 0);
  goto L4873;

 L149: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 27;
    }
  x1 = XEXP (x0, 0);
  goto L4873;

 L180: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DFmode)
    goto L5084;
  x1 = XEXP (x0, 0);
  goto L4876;

 L5084: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case FLOAT_EXTEND:
      goto L181;
    case FLOAT_TRUNCATE:
      goto L204;
    case UNSIGNED_FLOAT:
      goto L249;
    case PLUS:
      goto L894;
    case MINUS:
      goto L905;
    case MULT:
      goto L916;
    case ABS:
      goto L927;
    case NEG:
      goto L931;
    case SMIN:
      goto L940;
    case SMAX:
      goto L945;
    case DIV:
      goto L1036;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4876;

 L181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 35;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L204: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5096;
  x1 = XEXP (x0, 0);
  goto L4876;

 L5096: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L1075;
    case MINUS:
      goto L1095;
    case MULT:
      goto L1115;
    case NEG:
      goto L1269;
    case SUBREG:
    case REG:
      goto L5095;
    default:
      x1 = XEXP (x0, 0);
      goto L4876;
   }
 L5095: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L205;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1075: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5101;
  x1 = XEXP (x0, 0);
  goto L4876;

 L5101: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1197;
    case NEG:
      goto L1297;
    case REG:
    case CONST_DOUBLE:
      goto L5100;
    default:
      x1 = XEXP (x0, 0);
      goto L4876;
   }
 L5100: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1076;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1197: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1198;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1198: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1199;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1199: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1200;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1200: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 172;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1297: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L1298;
  x1 = XEXP (x0, 0);
  goto L4876;

 L1298: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1299;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1299: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1300;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1300: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1301;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1301: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 183;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1076: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1077;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1077: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 155;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1095: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5104;
  x1 = XEXP (x0, 0);
  goto L4876;

 L5104: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L1246;
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1096;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1246: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1247;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1247: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1248;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1248: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1249;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1249: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 177;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1096: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1097;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1097: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 158;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1115: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1116;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1116: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1117;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1117: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 161;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1269: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1270;
  x1 = XEXP (x0, 0);
  goto L4876;

 L1270: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1271;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1271: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1272;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1272: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 180;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L205: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 40;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L249: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 49;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L894: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L5106;
  x1 = XEXP (x0, 0);
  goto L4876;

 L5106: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L951;
    case NEG:
      goto L994;
    case SUBREG:
    case REG:
      goto L5105;
    default:
      x1 = XEXP (x0, 0);
      goto L4876;
   }
 L5105: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L895;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L951: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L952;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L952: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L953;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L953: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 142;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L994: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L995;
  x1 = XEXP (x0, 0);
  goto L4876;

 L995: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L996;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L996: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L997;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L997: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 148;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L895: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 131;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L905: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L5109;
  x1 = XEXP (x0, 0);
  goto L4876;

 L5109: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L966;
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L906;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L966: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L967;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L967: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L968;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L968: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 144;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L906: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 133;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L916: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L917;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L917: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 135;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L927: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 137;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L931: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L5111;
  x1 = XEXP (x0, 0);
  goto L4876;

 L5111: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L936;
    case MULT:
      goto L981;
    case SUBREG:
    case REG:
      goto L5110;
    default:
      x1 = XEXP (x0, 0);
      goto L4876;
   }
 L5110: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 138;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L936: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 139;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L981: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L982;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L982: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 146;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L940: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L941;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L941: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 140;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L945: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L946;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L946: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 141;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1036: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1037;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1037: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L1038;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L1038: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 4;
      return 151;
    }
 L1057: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 152;
    }
  x1 = XEXP (x0, 0);
  goto L4876;

 L213: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DFmode
      && GET_CODE (x1) == FLOAT)
    goto L214;
  x1 = XEXP (x0, 0);
  goto L1522;

 L214: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L215;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L215: ATTRIBUTE_UNUSED_LABEL
  if ((!INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 42;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L4870: ATTRIBUTE_UNUSED_LABEL
  if (destination_tfmode_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L152;
    }
 L4874: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L184;
    }
 L4884: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L1784;
    }
 L4885: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L1788;
    }
  goto L1522;

 L152: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_tfmode_operand (x1, TFmode))
    {
      operands[1] = x1;
      goto L153;
    }
  x1 = XEXP (x0, 0);
  goto L4874;

 L153: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && ia64_move_ok (operands[0], operands[1])))
    {
      return 28;
    }
  x1 = XEXP (x0, 0);
  goto L4874;

 L184: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == TFmode)
    goto L5113;
  x1 = XEXP (x0, 0);
  goto L4884;

 L5113: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case FLOAT_EXTEND:
      goto L185;
    case FLOAT:
      goto L209;
    case UNSIGNED_FLOAT:
      goto L253;
    case PLUS:
      goto L1061;
    case MINUS:
      goto L1081;
    case MULT:
      goto L1101;
    case ABS:
      goto L1150;
    case NEG:
      goto L1155;
    case SMIN:
      goto L1166;
    case SMAX:
      goto L1172;
    case DIV:
      goto L1347;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4884;

 L185: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L5124;
    case DFmode:
      goto L5125;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L5124: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L186;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L186: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 36;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L5125: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L191;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L191: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 37;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L209: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5126;
    case SImode:
      goto L5127;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L5126: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L210;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L210: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 41;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L5127: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == DIV)
    goto L763;
  x1 = XEXP (x0, 0);
  goto L4884;

 L763: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L764;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L764: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L765;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L765: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 4;
      return 115;
    }
 L786: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 116;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L253: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L254;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L254: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 50;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1061: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5129;
  x1 = XEXP (x0, 0);
  goto L4884;

 L5129: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L1179;
    case NEG:
      goto L1277;
    case REG:
    case CONST_DOUBLE:
      goto L5128;
    default:
      x1 = XEXP (x0, 0);
      goto L4884;
   }
 L5128: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1062;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1179: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1180;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1180: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1181;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1182;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1182: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 170;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1277: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1278;
  x1 = XEXP (x0, 0);
  goto L4884;

 L1278: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1279;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1279: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1280;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1280: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1281;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1281: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 181;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1062: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1063;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1063: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 153;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1081: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5132;
  x1 = XEXP (x0, 0);
  goto L4884;

 L5132: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L1228;
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1082;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1228: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1229;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1229: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1230;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1230: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1231;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1231: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 175;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1082: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1083;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1083: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 156;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1101: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1102;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1102: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1103;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1103: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 159;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1150: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1151;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1151: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 165;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1155: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5134;
  x1 = XEXP (x0, 0);
  goto L4884;

 L5134: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L1161;
    case MULT:
      goto L1254;
    case REG:
    case CONST_DOUBLE:
      goto L5133;
    default:
      x1 = XEXP (x0, 0);
      goto L4884;
   }
 L5133: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1156;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1161: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1162;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1162: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 167;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1254: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1255;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1255: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1256;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1256: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 178;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1156: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 166;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1166: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1167;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1167: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1168;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1168: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 168;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1172: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1173;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1173: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1174;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1174: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 169;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1347: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1348;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1348: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1349;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1349: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 5;
      return 186;
    }
 L1368: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 187;
    }
  x1 = XEXP (x0, 0);
  goto L4884;

 L1784: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == TFmode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 12)
    goto L1785;
  x1 = XEXP (x0, 0);
  goto L4885;

 L1785: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (register_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 244;
    }
  x1 = XEXP (x0, 0);
  goto L4885;

 L1788: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == TFmode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 13)
    goto L1789;
  x1 = XEXP (x0, 0);
  goto L1522;

 L1789: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (memory_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 245;
    }
  x1 = XEXP (x0, 0);
  goto L1522;

 L1727: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      return 236;
    }
  switch (GET_CODE (x1))
    {
    case IF_THEN_ELSE:
      goto L1524;
    case LABEL_REF:
      goto L1724;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1524: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[0] = x2;
      goto L1525;
    }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1525: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1526;
    }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1526: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L1527;
  x1 = XEXP (x0, 0);
  goto L1827;

 L1527: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1528;
    case PC:
      goto L1537;
    case RETURN:
      goto L1712;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1528: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[2] = x3;
  goto L1529;

 L1529: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC)
    {
      return 214;
    }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1537: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1538;
    case RETURN:
      goto L5136;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1538: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[2] = x3;
  return 215;

 L5136: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_direct_return ()))
    {
      return 234;
    }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1712: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC
      && (ia64_direct_return ()))
    {
      return 233;
    }
  x1 = XEXP (x0, 0);
  goto L1827;

 L1724: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  operands[0] = x2;
  return 235;

 L1828: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BLKmode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 18)
    goto L1829;
  x1 = XEXP (x0, 0);
  goto L1883;

 L1829: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  operands[1] = x2;
  return 263;

 L4886: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L1884;
    }
  goto ret0;

 L1884: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BImode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 15)
    goto L1885;
  goto ret0;

 L1885: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (rtx_equal_p (x2, operands[0]))
    {
      return 270;
    }
  goto ret0;

 L4887: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1892;
    }
  goto ret0;

 L1892: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 24)
    goto L1893;
  goto ret0;

 L1893: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_MODE (x2) == SImode)
    goto L5138;
  goto ret0;

 L5138: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1898;
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 273;
    }
  goto ret0;

 L1898: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode)
    goto L5139;
  goto ret0;

 L5139: ATTRIBUTE_UNUSED_LABEL
  if (basereg_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1899;
    }
 L5140: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1906;
    }
  goto ret0;

 L1899: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_14bit_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1900;
    }
  x3 = XEXP (x2, 0);
  goto L5140;

 L1900: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 274;
    }
  x1 = XEXP (x0, 1);
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5140;

 L1906: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (basereg_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1907;
    }
  goto ret0;

 L1907: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 275;
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

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TImode:
      goto L5144;
    case DImode:
      goto L5146;
    case BImode:
      goto L5147;
    case DFmode:
      goto L5149;
    case TFmode:
      goto L5150;
    case SFmode:
      goto L5151;
    default:
      break;
    }
 L1541: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PC)
    goto L1542;
 L1634: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L1635;
 L1737: ATTRIBUTE_UNUSED_LABEL
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5152;
    case SImode:
      goto L5154;
    default:
      break;
    }
 L1730: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PC)
    goto L1731;
  goto ret0;

 L5144: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, TImode))
    {
      operands[0] = x2;
      goto L131;
    }
  goto L1541;

 L131: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, TImode))
    {
      operands[1] = x2;
      goto L132;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L132: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L133;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L133: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L134;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L134: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 24;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L5146: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L306;
  if (fr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L237;
    }
 L5148: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L695;
    }
  goto L1541;

 L306: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L307;
    }
  goto L1541;

 L307: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32L)
    goto L308;
  goto L1541;

 L308: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L309;
  goto L1541;

 L309: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L310;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L310: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L311;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L311: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 59;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L237: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5156;
  x2 = XEXP (x1, 0);
  goto L5148;

 L5156: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FIX:
      goto L238;
    case UNSIGNED_FIX:
      goto L272;
    case PLUS:
      goto L678;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5148;

 L238: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L239;
    }
  x2 = XEXP (x1, 0);
  goto L5148;

 L239: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L240;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L240: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L241;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L241: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 47;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L272: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L273;
    }
  x2 = XEXP (x1, 0);
  goto L5148;

 L273: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L274;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L274: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L275;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L275: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 54;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L678: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L679;
  x2 = XEXP (x1, 0);
  goto L5148;

 L679: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (grfr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L680;
    }
  x2 = XEXP (x1, 0);
  goto L5148;

 L680: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (grfr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L681;
    }
  x2 = XEXP (x1, 0);
  goto L5148;

 L681: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L682;
    }
  x2 = XEXP (x1, 0);
  goto L5148;

 L682: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L683;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L683: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      return 109;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5148;

 L695: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L696;
  x2 = XEXP (x1, 0);
  goto L1541;

 L696: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L697;
  x2 = XEXP (x1, 0);
  goto L1541;

 L697: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L698;
  x2 = XEXP (x1, 0);
  goto L1541;

 L698: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L699;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L699: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L700;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L700: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L701;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L701: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[4] = x3;
      goto L702;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L702: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L703;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L703: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[5] = x2;
      goto L704;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L704: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 110;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L5147: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L360;
    }
  goto L1541;

 L360: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == NOT)
    goto L361;
  x2 = XEXP (x1, 0);
  goto L1541;

 L361: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L362;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L362: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L363;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L363: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[2] = x2;
      return 67;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L5149: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L1001;
    }
  goto L1541;

 L1001: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5159;
  x2 = XEXP (x1, 0);
  goto L1541;

 L5159: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L1002;
    case FLOAT_TRUNCATE:
      goto L1141;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1002: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == NEG)
    goto L1003;
  x2 = XEXP (x1, 0);
  goto L1541;

 L1003: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L1004;
  x2 = XEXP (x1, 0);
  goto L1541;

 L1004: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L1005;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1005: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L1006;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1006: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      goto L1007;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1007: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1008;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1008: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      return 149;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1141: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5161;
  x2 = XEXP (x1, 0);
  goto L1541;

 L5161: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1142;
    case PLUS:
      goto L1217;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1142: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1143;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1143: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1144;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1144: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1145;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1145: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1146;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1146: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 164;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1217: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5163;
  x2 = XEXP (x1, 0);
  goto L1541;

 L5163: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L1218;
    case NEG:
      goto L1320;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1218: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1219;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1219: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1220;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1220: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L1221;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1221: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1222;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1222: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1223;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1223: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 174;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1320: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L1321;
  x2 = XEXP (x1, 0);
  goto L1541;

 L1321: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L1322;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1322: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L1323;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1323: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L1324;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1324: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1325;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1325: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1326;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1326: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 185;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L5150: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L1121;
    }
  goto L1541;

 L1121: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5165;
  x2 = XEXP (x1, 0);
  goto L1541;

 L5165: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L1122;
    case PLUS:
      goto L1205;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1122: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1123;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1123: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1124;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1124: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1125;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1125: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1126;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1126: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 162;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1205: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5167;
  x2 = XEXP (x1, 0);
  goto L1541;

 L5167: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1206;
    case NEG:
      goto L1307;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1206: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1207;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1207: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1208;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1208: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1209;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1209: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1210;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1210: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1211;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1211: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 173;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1307: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L1308;
  x2 = XEXP (x1, 0);
  goto L1541;

 L1308: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1309;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1309: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1310;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1310: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1311;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1311: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1312;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1312: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1313;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1313: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 184;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L5151: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L1130;
    }
  goto L1541;

 L1130: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode
      && GET_CODE (x2) == FLOAT_TRUNCATE)
    goto L1131;
  x2 = XEXP (x1, 0);
  goto L1541;

 L1131: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1132;
  x2 = XEXP (x1, 0);
  goto L1541;

 L1132: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1133;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1133: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1134;
    }
  x2 = XEXP (x1, 0);
  goto L1541;

 L1134: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1135;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1135: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1136;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1136: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 163;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1541;

 L1542: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == IF_THEN_ELSE)
    goto L1543;
  x2 = XEXP (x1, 0);
  goto L1634;

 L1543: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == NE)
    goto L1544;
  x2 = XEXP (x1, 0);
  goto L1634;

 L1544: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (ar_lc_reg_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L1545;
    }
  x2 = XEXP (x1, 0);
  goto L1634;

 L1545: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L1546;
  x2 = XEXP (x1, 0);
  goto L1634;

 L1546: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == LABEL_REF)
    goto L1547;
  x2 = XEXP (x1, 0);
  goto L1634;

 L1547: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  operands[1] = x4;
  goto L1548;

 L1548: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == PC)
    goto L1549;
  x2 = XEXP (x1, 0);
  goto L1634;

 L1549: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1550;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1550: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[0]))
    goto L1551;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1551: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == IF_THEN_ELSE)
    goto L1552;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1552: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == NE)
    goto L1553;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1553: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (rtx_equal_p (x4, operands[0]))
    goto L1554;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1554: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L1555;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1555: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L1556;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1556: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (rtx_equal_p (x4, operands[0]))
    goto L1557;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1557: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == -1L)
    goto L1558;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1558: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (rtx_equal_p (x3, operands[0]))
    {
      return 216;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1634;

 L1635: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L1636;
  x2 = XEXP (x1, 0);
  goto L1737;

 L1636: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L1637;
  x2 = XEXP (x1, 0);
  goto L1737;

 L1637: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L1638;
    }
  x2 = XEXP (x1, 0);
  goto L1737;

 L1638: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT)
    goto L5169;
  x2 = XEXP (x1, 0);
  goto L1737;

 L5169: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x3, 0) == XWINT (x3, 0))
    switch ((int) XWINT (x3, 0))
      {
      case 0L:
        goto L1639;
      case 1L:
        goto L1684;
      default:
        break;
      }
  x2 = XEXP (x1, 0);
  goto L1737;

 L1639: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1640;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1737;

 L1640: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 226;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1737;

 L1684: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1685;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1737;

 L1685: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1686;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1737;

 L1686: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 229;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1737;

 L5152: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1738;
    }
 L5153: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1769;
    }
 L5155: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1878;
    }
  goto L1730;

 L1738: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5171;
  x2 = XEXP (x1, 0);
  goto L5153;

 L5171: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L1739;
    case UNSPEC:
      goto L5174;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5172;
    default:
      x2 = XEXP (x1, 0);
      goto L5153;
   }
 L5172: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1748;
    }
  x2 = XEXP (x1, 0);
  goto L5153;

 L1739: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1740;
    }
  x2 = XEXP (x1, 0);
  goto L5153;

 L1740: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1741;
    }
  x2 = XEXP (x1, 0);
  goto L5153;

 L1741: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1742;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L1742: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1743;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L1743: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (rtx_equal_p (x2, operands[3]))
    {
      return 238;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L5174: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 2
      && XINT (x2, 1) == 11)
    goto L1778;
  x2 = XEXP (x1, 0);
  goto L5153;

 L1778: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1779;
    }
  x2 = XEXP (x1, 0);
  goto L5153;

 L1779: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1780;
    }
  x2 = XEXP (x1, 0);
  goto L5153;

 L1780: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1781;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L1781: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      return 243;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L1748: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1749;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L1749: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[1]))
    goto L1750;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L1750: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (rtx_equal_p (x2, operands[1]))
    {
      return 239;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5153;

 L1769: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 2
      && XINT (x2, 1) == 10)
    goto L1770;
  x2 = XEXP (x1, 0);
  goto L5155;

 L1770: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1771;
    }
  x2 = XEXP (x1, 0);
  goto L5155;

 L1771: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1772;
    }
  x2 = XEXP (x1, 0);
  goto L5155;

 L1772: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1773;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5155;

 L1773: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      return 242;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5155;

 L1878: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (not_postinc_memory_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1879;
    }
 L1842: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x2, operands[1]))
    goto L1843;
  x2 = XEXP (x1, 0);
  goto L1730;

 L1879: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1880;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1842;

 L1880: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[1]))
    goto L1881;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1842;

 L1881: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 269;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1842;

 L1843: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1844;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1844: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (not_postinc_memory_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1845;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1845: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5175;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5175: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L5177;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5177: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x2, 0))
    {
    case 2:
      goto L5179;
    case 3:
      goto L5180;
    default:
      break;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5179: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 20)
    goto L1846;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1846: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1847;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1847: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (fetchadd_operand (x3, DImode))
    {
      operands[2] = x3;
      return 265;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5180: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 19)
    goto L1865;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1865: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1866;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1866: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1867;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1867: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 2);
  if (ar_ccv_reg_operand (x3, DImode))
    {
      operands[3] = x3;
      return 267;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5154: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L1871;
    }
  goto L1730;

 L1871: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (not_postinc_memory_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1872;
    }
 L1833: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x2, operands[1]))
    goto L1834;
  x2 = XEXP (x1, 0);
  goto L1730;

 L1872: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1873;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1833;

 L1873: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[1]))
    goto L1874;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1833;

 L1874: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 268;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1833;

 L1834: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1835;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1835: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (not_postinc_memory_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1836;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1836: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L5181;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5181: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L5183;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5183: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x2, 0))
    {
    case 2:
      goto L5185;
    case 3:
      goto L5186;
    default:
      break;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5185: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 20)
    goto L1837;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1837: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1838;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1838: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (fetchadd_operand (x3, SImode))
    {
      operands[2] = x3;
      return 264;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L5186: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 19)
    goto L1855;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1855: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1856;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1856: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1857;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1857: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 2);
  if (ar_ccv_reg_operand (x3, VOIDmode))
    {
      operands[3] = x3;
      return 266;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1730;

 L1731: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1732;
    }
  goto ret0;

 L1732: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1733;
  goto ret0;

 L1733: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == LABEL_REF)
    goto L1734;
  goto ret0;

 L1734: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  return 237;
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

  switch (XVECLEN (x0, 0))
    {
    case 3:
      goto L15;
    case 2:
      goto L129;
    case 5:
      goto L624;
    case 4:
      goto L767;
    case 6:
      goto L1328;
    default:
      break;
    }
  goto ret0;

 L15: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L16;
    case CALL:
      goto L1689;
    default:
     break;
   }
  goto ret0;

 L16: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L5141;
    case DImode:
      goto L5142;
    case TFmode:
      goto L5143;
    default:
      break;
    }
  goto ret0;

 L5141: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L17;
    }
  goto ret0;

 L17: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (symbolic_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L18;
    }
  goto ret0;

 L18: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L19;
  goto ret0;

 L19: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L20;
    }
  goto ret0;

 L20: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L21;
  goto ret0;

 L21: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1)
    {
      return 4;
    }
  goto ret0;

 L5142: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L29;
    }
  goto ret0;

 L29: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L30;
    }
  goto ret0;

 L30: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L31;
  goto ret0;

 L31: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L32;
    }
  goto ret0;

 L32: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L33;
  goto ret0;

 L33: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1)
    {
      return 6;
    }
  goto ret0;

 L5143: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L1372;
    }
  goto ret0;

 L1372: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == DIV)
    goto L1373;
  goto ret0;

 L1373: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    goto L1374;
  goto ret0;

 L1374: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1375;
    }
  goto ret0;

 L1375: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1376;
  goto ret0;

 L1376: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1377;
    }
  goto ret0;

 L1377: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 2
      && XINT (x2, 1) == 14)
    goto L1378;
  goto ret0;

 L1378: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1379;
    }
  goto ret0;

 L1379: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (rtx_equal_p (x3, operands[3]))
    goto L1380;
  goto ret0;

 L1380: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1381;
  goto ret0;

 L1381: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1382;
    }
  goto ret0;

 L1382: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 188;
    }
  goto ret0;

 L1689: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L1690;
  goto ret0;

 L1690: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L1691;
    }
  goto ret0;

 L1691: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    goto L1692;
  goto ret0;

 L1692: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1693;
  goto ret0;

 L1693: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1694;
    }
  goto ret0;

 L1694: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1695;
  goto ret0;

 L1695: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      return 230;
    }
  goto ret0;

 L129: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L130;
    case CALL:
      goto L1627;
    case RETURN:
      goto L1702;
    default:
     break;
   }
  goto ret0;

 L130: ATTRIBUTE_UNUSED_LABEL
  return recog_5 (x0, insn, pnum_clobbers);

 L1627: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L1628;
 L1658: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MEM)
    goto L1659;
  goto ret0;

 L1628: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L1629;
    }
  goto L1658;

 L1629: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L1630;
  x2 = XEXP (x1, 0);
  goto L1658;

 L1630: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1631;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1658;

 L1631: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 225;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1658;

 L1659: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L1660;
    }
  goto ret0;

 L1660: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    goto L1661;
  goto ret0;

 L1661: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1662;
  goto ret0;

 L1662: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1663;
    }
  goto ret0;

 L1663: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 228;
    }
  goto ret0;

 L1702: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1703;
  goto ret0;

 L1703: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      return 231;
    }
  goto ret0;

 L624: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L625;
  goto ret0;

 L625: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5187;
    case DFmode:
      goto L5188;
    case DImode:
      goto L5189;
    default:
      break;
    }
  goto ret0;

 L5187: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L626;
    }
  goto ret0;

 L626: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == FLOAT)
    goto L627;
  goto ret0;

 L627: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L628;
  goto ret0;

 L628: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L629;
    }
  goto ret0;

 L629: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L630;
    }
  goto ret0;

 L630: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L631;
  goto ret0;

 L631: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5190;
  goto ret0;

 L5190: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L632;
    }
 L5191: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L752;
    }
  goto ret0;

 L632: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L633;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5191;

 L633: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L634;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5191;

 L634: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L635;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5191;

 L635: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L636;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5191;

 L636: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == USE)
    goto L637;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5191;

 L637: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L638;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5191;

 L638: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV))
    {
      return 102;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5191;

 L752: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L753;
  goto ret0;

 L753: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L754;
    }
  goto ret0;

 L754: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L755;
  goto ret0;

 L755: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L756;
    }
  goto ret0;

 L756: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L757;
  goto ret0;

 L757: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L758;
    }
  goto ret0;

 L758: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT))
    {
      return 115;
    }
  goto ret0;

 L5188: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L1021;
    }
  goto ret0;

 L1021: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L1022;
  goto ret0;

 L1022: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L1023;
    }
  goto ret0;

 L1023: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L1024;
    }
  goto ret0;

 L1024: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1025;
  goto ret0;

 L1025: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1026;
    }
  goto ret0;

 L1026: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1027;
  goto ret0;

 L1027: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L1028;
    }
  goto ret0;

 L1028: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1029;
  goto ret0;

 L1029: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L1030;
    }
  goto ret0;

 L1030: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L1031;
  goto ret0;

 L1031: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L1032;
    }
  goto ret0;

 L1032: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT))
    {
      return 151;
    }
  goto ret0;

 L5189: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1756;
    }
  goto ret0;

 L1756: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC_VOLATILE
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 0)
    goto L1757;
  goto ret0;

 L1757: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L1758;
  goto ret0;

 L1758: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1759;
  goto ret0;

 L1759: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1760;
    }
  goto ret0;

 L1760: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1761;
  goto ret0;

 L1761: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1762;
    }
  goto ret0;

 L1762: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == USE)
    goto L1763;
  goto ret0;

 L1763: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1764;
    }
  goto ret0;

 L1764: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == USE)
    goto L1765;
  goto ret0;

 L1765: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[4] = x2;
      return 241;
    }
  goto ret0;

 L767: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L768;
    case CALL:
      goto L1647;
    default:
     break;
   }
  goto ret0;

 L768: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5192;
    case SFmode:
      goto L5193;
    case DFmode:
      goto L5194;
    default:
      break;
    }
 L1666: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L1667;

 L5192: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L769;
    }
  goto L1666;

 L769: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5195;
  x2 = XEXP (x1, 0);
  goto L1666;

 L5195: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT:
      goto L770;
    case DIV:
      goto L1354;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1666;

 L770: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L771;
  x2 = XEXP (x1, 0);
  goto L1666;

 L771: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L772;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L772: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L773;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L773: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L774;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L774: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L775;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L775: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L776;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L776: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L777;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L777: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L778;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L778: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L779;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L779: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR))
    {
      return 116;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1354: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1355;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L1355: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1356;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L1356: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1357;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1357: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1358;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1358: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1359;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1359: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L1360;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1360: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1361;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1361: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L1362;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1362: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR))
    {
      return 187;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L5193: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L856;
    }
  goto L1666;

 L856: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode
      && GET_CODE (x2) == DIV)
    goto L857;
  x2 = XEXP (x1, 0);
  goto L1666;

 L857: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L858;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L858: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L859;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L859: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L860;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L860: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L861;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L861: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L862;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L862: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L863;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L863: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L864;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L864: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L865;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L865: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT))
    {
      return 129;
    }
 L884: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR))
    {
      return 130;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L5194: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L1042;
    }
  goto L1666;

 L1042: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L1043;
  x2 = XEXP (x1, 0);
  goto L1666;

 L1043: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L1044;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L1044: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L1045;
    }
  x2 = XEXP (x1, 0);
  goto L1666;

 L1045: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1046;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1046: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1047;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1047: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1048;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1048: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      operands[4] = x2;
      goto L1049;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1049: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1050;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1050: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L1051;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1051: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR))
    {
      return 152;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1666;

 L1667: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L1668;
  goto ret0;

 L1668: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L1669;
  goto ret0;

 L1669: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L1670;
    }
  goto ret0;

 L1670: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    goto L1671;
  goto ret0;

 L1671: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1672;
  goto ret0;

 L1672: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1673;
    }
  goto ret0;

 L1673: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1674;
  goto ret0;

 L1674: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1675;
    }
  goto ret0;

 L1675: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1676;
  goto ret0;

 L1676: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      return 229;
    }
  goto ret0;

 L1647: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == MEM)
    goto L1648;
  goto ret0;

 L1648: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L1649;
    }
  goto ret0;

 L1649: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    goto L1650;
  goto ret0;

 L1650: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1651;
  goto ret0;

 L1651: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1652;
    }
  goto ret0;

 L1652: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1653;
  goto ret0;

 L1653: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1654;
    }
  goto ret0;

 L1654: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1655;
  goto ret0;

 L1655: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      return 228;
    }
  goto ret0;

 L1328: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L1329;
  goto ret0;

 L1329: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L1330;
    }
  goto ret0;

 L1330: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == DIV)
    goto L1331;
  goto ret0;

 L1331: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1332;
    }
  goto ret0;

 L1332: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1333;
    }
  goto ret0;

 L1333: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1334;
  goto ret0;

 L1334: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1335;
    }
  goto ret0;

 L1335: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1336;
  goto ret0;

 L1336: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L1337;
    }
  goto ret0;

 L1337: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1338;
  goto ret0;

 L1338: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L1339;
    }
  goto ret0;

 L1339: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L1340;
  goto ret0;

 L1340: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[6] = x2;
      goto L1341;
    }
  goto ret0;

 L1341: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == CLOBBER)
    goto L1342;
  goto ret0;

 L1342: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[7] = x2;
      goto L1343;
    }
  goto ret0;

 L1343: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT))
    {
      return 186;
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

  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case BImode:
      goto L5216;
    case QImode:
      goto L5217;
    case HImode:
      goto L5218;
    case SImode:
      goto L5219;
    case DImode:
      goto L5233;
    case SFmode:
      goto L5222;
    case DFmode:
      goto L5223;
    case TFmode:
      goto L5224;
    default:
      break;
    }
  goto ret0;

 L5216: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2376;
    }
  goto ret0;

 L2376: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, BImode))
    {
      operands[1] = x2;
      return 420;
    }
  goto ret0;

 L5217: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, QImode))
    {
      operands[0] = x2;
      goto L2383;
    }
  goto ret0;

 L2383: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L2384;
    }
  goto ret0;

 L2384: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 421;
    }
  goto ret0;

 L5218: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, HImode))
    {
      operands[0] = x2;
      goto L2391;
    }
  goto ret0;

 L2391: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L2392;
    }
  goto ret0;

 L2392: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 422;
    }
  goto ret0;

 L5219: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L2411;
    }
  goto ret0;

 L2411: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2412;
    }
  goto ret0;

 L2412: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 424;
    }
  goto ret0;

 L5233: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L2899;
  if (destination_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2431;
    }
 L5221: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2439;
    }
 L5225: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2617;
    }
 L5226: ATTRIBUTE_UNUSED_LABEL
  if (grfr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2633;
    }
 L5232: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2744;
    }
  goto ret0;

 L2899: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2900;
    }
  goto ret0;

 L2900: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32L)
    goto L2901;
  goto ret0;

 L2901: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L2902;
  goto ret0;

 L2902: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == LSHIFTRT)
    goto L2903;
  goto ret0;

 L2903: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2904;
    }
  goto ret0;

 L2904: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32L)
    {
      return 477;
    }
  goto ret0;

 L2431: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2432;
    }
  x2 = XEXP (x1, 0);
  goto L5221;

 L2432: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 426;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5221;

 L2439: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5234;
  x2 = XEXP (x1, 0);
  goto L5225;

 L5234: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2440;
    case MINUS:
      goto L2458;
    case UNSPEC:
      goto L5238;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5225;

 L2440: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    goto L2487;
  x2 = XEXP (x1, 0);
  goto L5225;

 L2487: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5240;
 L2441: ATTRIBUTE_UNUSED_LABEL
  if (function_operand (x3, VOIDmode))
    {
      operands[1] = x3;
      return 427;
    }
 L2450: ATTRIBUTE_UNUSED_LABEL
  if (sdata_symbolic_operand (x3, VOIDmode))
    {
      operands[1] = x3;
      return 428;
    }
  x2 = XEXP (x1, 0);
  goto L5225;

 L5240: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == UNSPEC)
    goto L5243;
  goto L2441;

 L5243: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x3, 0) == 1)
    goto L5246;
  goto L2441;

 L5246: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x3, 1))
    {
    case 0L:
      goto L2488;
    case 1L:
      goto L2498;
    case 3L:
      goto L2547;
    default:
      break;
    }
  goto L2441;

 L2488: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[1] = x4;
      return 432;
    }
  goto L2441;

 L2498: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[1] = x4;
      return 433;
    }
  goto L2441;

 L2547: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[1] = x4;
      return 438;
    }
  goto L2441;

 L2458: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2459;
    }
  x2 = XEXP (x1, 0);
  goto L5225;

 L2459: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    {
      return 429;
    }
  x2 = XEXP (x1, 0);
  goto L5225;

 L5238: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1)
    goto L5249;
  x2 = XEXP (x1, 0);
  goto L5225;

 L5249: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x2, 1))
    {
    case 2L:
      goto L2506;
    case 4L:
      goto L2555;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5225;

 L2506: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_MODE (x3) == DImode)
    goto L5251;
  x2 = XEXP (x1, 0);
  goto L5225;

 L5251: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2507;
    }
 L5252: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      return 435;
    }
  x2 = XEXP (x1, 0);
  goto L5225;

 L2507: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 434;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  x3 = XVECEXP (x2, 0, 0);
  goto L5252;

 L2555: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_MODE (x3) == DImode)
    goto L5253;
  x2 = XEXP (x1, 0);
  goto L5225;

 L5253: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2556;
    }
 L5254: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      return 440;
    }
  x2 = XEXP (x1, 0);
  goto L5225;

 L2556: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 439;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  x3 = XVECEXP (x2, 0, 0);
  goto L5254;

 L2617: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5255;
  x2 = XEXP (x1, 0);
  goto L5226;

 L5255: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case SIGN_EXTEND:
      goto L2618;
    case ZERO_EXTEND:
      goto L2642;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5226;

 L2618: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case QImode:
      goto L5257;
    case HImode:
      goto L5258;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5226;

 L5257: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, QImode))
    {
      operands[1] = x3;
      return 446;
    }
  x2 = XEXP (x1, 0);
  goto L5226;

 L5258: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, HImode))
    {
      operands[1] = x3;
      return 447;
    }
  x2 = XEXP (x1, 0);
  goto L5226;

 L2642: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case QImode:
      goto L5259;
    case HImode:
      goto L5260;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5226;

 L5259: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x3, QImode))
    {
      operands[1] = x3;
      return 449;
    }
  x2 = XEXP (x1, 0);
  goto L5226;

 L5260: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x3, HImode))
    {
      operands[1] = x3;
      return 450;
    }
  x2 = XEXP (x1, 0);
  goto L5226;

 L2633: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5261;
  x2 = XEXP (x1, 0);
  goto L5232;

 L5261: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case SIGN_EXTEND:
      goto L2634;
    case ZERO_EXTEND:
      goto L2658;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5232;

 L2634: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 448;
    }
  x2 = XEXP (x1, 0);
  goto L5232;

 L2658: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_nonimmediate_operand (x3, SImode))
    {
      operands[1] = x3;
      return 451;
    }
  x2 = XEXP (x1, 0);
  goto L5232;

 L2744: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5263;
  goto ret0;

 L5263: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FIX:
      goto L2745;
    case UNSIGNED_FIX:
      goto L2807;
    default:
     break;
   }
  goto ret0;

 L2745: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5265;
    case DFmode:
      goto L5266;
    case TFmode:
      goto L5267;
    default:
      break;
    }
  goto ret0;

 L5265: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 461;
    }
  goto ret0;

 L5266: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 462;
    }
  goto ret0;

 L5267: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2762;
    }
  goto ret0;

 L2762: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 463;
    }
  goto ret0;

 L2807: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5268;
    case DFmode:
      goto L5269;
    case TFmode:
      goto L5270;
    default:
      break;
    }
  goto ret0;

 L5268: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 468;
    }
  goto ret0;

 L5269: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 469;
    }
  goto ret0;

 L5270: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2824;
    }
  goto ret0;

 L2824: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 470;
    }
  goto ret0;

 L5222: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L2593;
    }
 L5229: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L2691;
    }
 L5231: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L2735;
    }
  goto ret0;

 L2593: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2594;
    }
  x2 = XEXP (x1, 0);
  goto L5229;

 L2594: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 443;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5229;

 L2691: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5271;
  x2 = XEXP (x1, 0);
  goto L5231;

 L5271: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT_TRUNCATE:
      goto L2692;
    case UNSIGNED_FLOAT:
      goto L2782;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5231;

 L2692: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DFmode:
      goto L5273;
    case TFmode:
      goto L5274;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5231;

 L5273: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 455;
    }
  x2 = XEXP (x1, 0);
  goto L5231;

 L5274: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2701;
    }
  x2 = XEXP (x1, 0);
  goto L5231;

 L2701: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 456;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5231;

 L2782: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 465;
    }
  x2 = XEXP (x1, 0);
  goto L5231;

 L2735: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode
      && GET_CODE (x2) == FLOAT)
    goto L2736;
  goto ret0;

 L2736: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2737;
    }
  goto ret0;

 L2737: ATTRIBUTE_UNUSED_LABEL
  if ((!INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 460;
    }
  goto ret0;

 L5223: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2601;
    }
 L5227: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2665;
    }
 L5230: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2726;
    }
  goto ret0;

 L2601: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2602;
    }
  x2 = XEXP (x1, 0);
  goto L5227;

 L2602: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 444;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5227;

 L2665: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5275;
  x2 = XEXP (x1, 0);
  goto L5230;

 L5275: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT_EXTEND:
      goto L2666;
    case FLOAT_TRUNCATE:
      goto L2709;
    case UNSIGNED_FLOAT:
      goto L2790;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5230;

 L2666: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 452;
    }
  x2 = XEXP (x1, 0);
  goto L5230;

 L2709: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2710;
    }
  x2 = XEXP (x1, 0);
  goto L5230;

 L2710: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 457;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5230;

 L2790: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 466;
    }
  x2 = XEXP (x1, 0);
  goto L5230;

 L2726: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == FLOAT)
    goto L2727;
  goto ret0;

 L2727: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2728;
    }
  goto ret0;

 L2728: ATTRIBUTE_UNUSED_LABEL
  if ((!INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 459;
    }
  goto ret0;

 L5224: ATTRIBUTE_UNUSED_LABEL
  if (destination_tfmode_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2609;
    }
 L5228: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2673;
    }
  goto ret0;

 L2609: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_tfmode_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L2610;
    }
  x2 = XEXP (x1, 0);
  goto L5228;

 L2610: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && ia64_move_ok (operands[0], operands[1])))
    {
      return 445;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5228;

 L2673: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5278;
  goto ret0;

 L5278: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT_EXTEND:
      goto L2674;
    case FLOAT:
      goto L2718;
    case UNSIGNED_FLOAT:
      goto L2798;
    default:
     break;
   }
  goto ret0;

 L2674: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5281;
    case DFmode:
      goto L5282;
    default:
      break;
    }
  goto ret0;

 L5281: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L2675;
    }
  goto ret0;

 L2675: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 453;
    }
  goto ret0;

 L5282: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L2684;
    }
  goto ret0;

 L2684: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 454;
    }
  goto ret0;

 L2718: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2719;
    }
  goto ret0;

 L2719: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 458;
    }
  goto ret0;

 L2798: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2799;
    }
  goto ret0;

 L2799: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 467;
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

  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5292;
    case BImode:
      goto L5294;
    case HImode:
      goto L5295;
    case SImode:
      goto L5296;
    case SFmode:
      goto L5299;
    case DFmode:
      goto L5300;
    case TFmode:
      goto L5301;
    default:
      break;
    }
  goto ret0;

 L5292: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2466;
    }
 L5293: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2921;
    }
 L5298: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L3423;
    }
 L5302: ATTRIBUTE_UNUSED_LABEL
  if (grfr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4334;
    }
  goto ret0;

 L2466: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5303;
  x2 = XEXP (x1, 0);
  goto L5293;

 L5303: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2467;
    case LO_SUM:
      goto L2477;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2467: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L5305;
  x2 = XEXP (x1, 0);
  goto L5293;

 L5305: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == HIGH)
    goto L2468;
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2524;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2468: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (got_symbolic_operand (x4, VOIDmode))
    {
      operands[1] = x4;
      goto L2469;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2469: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 430;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2524: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5307;
  x2 = XEXP (x1, 0);
  goto L5293;

 L5307: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == UNSPEC)
    goto L5309;
  x2 = XEXP (x1, 0);
  goto L5293;

 L5309: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x3, 0) == 1)
    goto L5311;
  x2 = XEXP (x1, 0);
  goto L5293;

 L5311: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x3, 1))
    {
    case 2L:
      goto L2525;
    case 4L:
      goto L2574;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2525: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2526;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2526: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS14))
    {
      return 436;
    }
 L2537: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS22))
    {
      return 437;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5293;

 L2574: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2575;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2575: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS14))
    {
      return 441;
    }
 L2586: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS22))
    {
      return 442;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5293;

 L2477: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2478;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2478: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (got_symbolic_operand (x3, VOIDmode))
    {
      operands[2] = x3;
      return 431;
    }
  x2 = XEXP (x1, 0);
  goto L5293;

 L2921: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5313;
  x2 = XEXP (x1, 0);
  goto L5298;

 L5313: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case IOR:
      goto L2922;
    case PLUS:
      goto L3374;
    case MINUS:
      goto L3405;
    case ASHIFT:
      goto L4257;
    case ASHIFTRT:
      goto L4291;
    case LSHIFTRT:
      goto L4300;
    case ROTATERT:
      goto L4309;
    case ROTATE:
      goto L4318;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5298;

 L2922: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTEND)
    goto L2923;
  x2 = XEXP (x1, 0);
  goto L5298;

 L2923: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L2924;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L2924: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ASHIFT)
    goto L2925;
  x2 = XEXP (x1, 0);
  goto L5298;

 L2925: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTEND)
    goto L2926;
  x2 = XEXP (x1, 0);
  goto L5298;

 L2926: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L2927;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L2927: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 32L)
    {
      return 479;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3374: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L5322;
  x2 = XEXP (x1, 0);
  goto L5298;

 L5322: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3384;
    case NOT:
      goto L3415;
    case SUBREG:
    case REG:
      goto L5321;
    default:
      x2 = XEXP (x1, 0);
      goto L5298;
   }
 L5321: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3375;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3384: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L3385;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3385: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3386;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3386: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    {
      return 520;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3415: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L3416;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3416: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 523;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3375: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 519;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3405: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3406;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3406: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 522;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4257: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4258;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4258: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_6bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 599;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4291: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4292;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4292: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_6bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 602;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4300: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4301;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4301: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_6bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 603;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4309: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4310;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4310: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (shift_count_operand (x3, DImode))
    {
      operands[2] = x3;
      return 604;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4318: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4319;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L4319: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (shift_count_operand (x3, DImode))
    {
      operands[2] = x3;
      return 605;
    }
  x2 = XEXP (x1, 0);
  goto L5298;

 L3423: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5324;
  x2 = XEXP (x1, 0);
  goto L5302;

 L5324: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L3424;
    case TRUNCATE:
      goto L3464;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3424: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3425;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3425: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 524;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3464: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode
      && GET_CODE (x3) == LSHIFTRT)
    goto L3465;
  x2 = XEXP (x1, 0);
  goto L5302;

 L3465: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == MULT)
    goto L3466;
  x2 = XEXP (x1, 0);
  goto L5302;

 L3466: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TImode)
    goto L5326;
  x2 = XEXP (x1, 0);
  goto L5302;

 L5326: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x5))
    {
    case SIGN_EXTEND:
      goto L3467;
    case ZERO_EXTEND:
      goto L3481;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3467: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L3468;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3468: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode
      && GET_CODE (x5) == SIGN_EXTEND)
    goto L3469;
  x2 = XEXP (x1, 0);
  goto L5302;

 L3469: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L3470;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3470: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 64L)
    {
      return 527;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3481: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L3482;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3482: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode
      && GET_CODE (x5) == ZERO_EXTEND)
    goto L3483;
  x2 = XEXP (x1, 0);
  goto L5302;

 L3483: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L3484;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L3484: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 64L)
    {
      return 528;
    }
  x2 = XEXP (x1, 0);
  goto L5302;

 L4334: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5328;
  goto ret0;

 L5328: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L4335;
    case IOR:
      goto L4354;
    case XOR:
      goto L4363;
    default:
     break;
   }
  goto ret0;

 L4335: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L5332;
  goto ret0;

 L5332: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == NOT)
    goto L4345;
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4336;
    }
  goto ret0;

 L4345: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (grfr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4346;
    }
  goto ret0;

 L4346: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 608;
    }
  goto ret0;

 L4336: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 607;
    }
  goto ret0;

 L4354: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4355;
    }
  goto ret0;

 L4355: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 609;
    }
  goto ret0;

 L4363: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4364;
    }
  goto ret0;

 L4364: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 610;
    }
  goto ret0;

 L5294: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2934;
    }
  goto ret0;

 L2934: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode)
    goto L5333;
  goto ret0;

 L5333: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2935;
    case IOR:
      goto L2954;
    case EQ:
      goto L4444;
    case NE:
      goto L4456;
    default:
     break;
   }
  goto ret0;

 L2935: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5338;
  goto ret0;

 L5338: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L2945;
    case NE:
      goto L3077;
    case EQ:
      goto L3090;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5337;
    default:
      goto ret0;
   }
 L5337: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2936;
    }
  goto ret0;

 L2945: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2946;
    }
  goto ret0;

 L2946: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 481;
    }
  goto ret0;

 L3077: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L3078;
  goto ret0;

 L3078: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3079;
    }
  goto ret0;

 L3079: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3080;
  goto ret0;

 L3080: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3081;
  goto ret0;

 L3081: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 493;
    }
  goto ret0;

 L3090: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L3091;
  goto ret0;

 L3091: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3092;
    }
  goto ret0;

 L3092: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3093;
  goto ret0;

 L3093: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3094;
  goto ret0;

 L3094: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 494;
    }
  goto ret0;

 L2936: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 480;
    }
  goto ret0;

 L2954: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5342;
  goto ret0;

 L5342: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L2964;
    case NE:
      goto L3223;
    case EQ:
      goto L3236;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5341;
    default:
      goto ret0;
   }
 L5341: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2955;
    }
  goto ret0;

 L2964: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2965;
    }
  goto ret0;

 L2965: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 483;
    }
  goto ret0;

 L3223: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L3224;
  goto ret0;

 L3224: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3225;
    }
  goto ret0;

 L3225: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3226;
  goto ret0;

 L3226: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3227;
  goto ret0;

 L3227: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 505;
    }
  goto ret0;

 L3236: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L3237;
  goto ret0;

 L3237: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3238;
    }
  goto ret0;

 L3238: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3239;
  goto ret0;

 L3239: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3240;
  goto ret0;

 L3240: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 506;
    }
  goto ret0;

 L2955: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 482;
    }
  goto ret0;

 L4444: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTRACT)
    goto L4445;
  goto ret0;

 L4445: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4446;
    }
  goto ret0;

 L4446: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L4447;
  goto ret0;

 L4447: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (immediate_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4448;
    }
  goto ret0;

 L4448: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    {
      return 619;
    }
  goto ret0;

 L4456: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTRACT)
    goto L4457;
  goto ret0;

 L4457: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4458;
    }
  goto ret0;

 L4458: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L4459;
  goto ret0;

 L4459: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (immediate_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4460;
    }
  goto ret0;

 L4460: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    {
      return 620;
    }
  goto ret0;

 L5295: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, HImode))
    {
      operands[0] = x2;
      goto L3275;
    }
  goto ret0;

 L3275: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == HImode
      && GET_CODE (x2) == MULT)
    goto L3276;
  goto ret0;

 L3276: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, HImode))
    {
      operands[1] = x3;
      goto L3277;
    }
  goto ret0;

 L3277: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, HImode))
    {
      operands[2] = x3;
      return 509;
    }
  goto ret0;

 L5296: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3284;
    }
 L5297: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3345;
    }
  goto ret0;

 L3284: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L5345;
  x2 = XEXP (x1, 0);
  goto L5297;

 L5345: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3285;
    case MINUS:
      goto L3327;
    case ASHIFT:
      goto L4230;
    case ROTATERT:
      goto L4239;
    case ROTATE:
      goto L4248;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3285: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode)
    goto L5351;
  x2 = XEXP (x1, 0);
  goto L5297;

 L5351: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3295;
    case NOT:
      goto L3337;
    case SUBREG:
    case REG:
      goto L5350;
    default:
      x2 = XEXP (x1, 0);
      goto L5297;
   }
 L5350: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3286;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3295: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3296;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3296: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3297;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3297: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    {
      return 511;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3337: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3338;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3338: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 515;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3286: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_22bit_operand (x3, SImode))
    {
      operands[2] = x3;
      return 510;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3327: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3328;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3328: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 514;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L4230: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4231;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L4231: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_5bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 596;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L4239: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4240;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L4240: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_5bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 597;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L4248: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4249;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L4249: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (shift_32bit_count_operand (x3, SImode))
    {
      operands[2] = x3;
      return 598;
    }
  x2 = XEXP (x1, 0);
  goto L5297;

 L3345: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MULT)
    goto L3346;
  goto ret0;

 L3346: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3347;
    }
  goto ret0;

 L3347: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 516;
    }
  goto ret0;

 L5299: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L3507;
    }
  goto ret0;

 L3507: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5353;
  goto ret0;

 L5353: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3508;
    case MINUS:
      goto L3517;
    case MULT:
      goto L3526;
    case SMIN:
      goto L3560;
    case SMAX:
      goto L3569;
    case NEG:
      goto L3600;
    case FLOAT_TRUNCATE:
      goto L3631;
    default:
     break;
   }
  goto ret0;

 L3508: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3509;
    }
  goto ret0;

 L3509: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 531;
    }
  goto ret0;

 L3517: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3518;
    }
  goto ret0;

 L3518: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 532;
    }
  goto ret0;

 L3526: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3527;
    }
  goto ret0;

 L3527: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 533;
    }
  goto ret0;

 L3560: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3561;
    }
  goto ret0;

 L3561: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 537;
    }
  goto ret0;

 L3569: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3570;
    }
  goto ret0;

 L3570: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 538;
    }
  goto ret0;

 L3600: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == MULT)
    goto L3601;
  goto ret0;

 L3601: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L3602;
    }
  goto ret0;

 L3602: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      return 541;
    }
  goto ret0;

 L3631: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DFmode:
      goto L5360;
    case TFmode:
      goto L5364;
    default:
      break;
    }
  goto ret0;

 L5360: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3632;
    case MINUS:
      goto L3651;
    case MULT:
      goto L3670;
    case NEG:
      goto L3779;
    default:
     break;
   }
  goto ret0;

 L3632: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3633;
    }
  goto ret0;

 L3633: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 544;
    }
  goto ret0;

 L3651: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3652;
    }
  goto ret0;

 L3652: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 546;
    }
  goto ret0;

 L3670: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3671;
    }
  goto ret0;

 L3671: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 548;
    }
  goto ret0;

 L3779: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L3780;
  goto ret0;

 L3780: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3781;
    }
  goto ret0;

 L3781: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      return 559;
    }
  goto ret0;

 L5364: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3840;
    case MINUS:
      goto L3872;
    case MULT:
      goto L3904;
    case NEG:
      goto L4133;
    default:
     break;
   }
  goto ret0;

 L3840: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3841;
    }
  goto ret0;

 L3841: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3842;
    }
  goto ret0;

 L3842: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 564;
    }
  goto ret0;

 L3872: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3873;
    }
  goto ret0;

 L3873: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3874;
    }
  goto ret0;

 L3874: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 567;
    }
  goto ret0;

 L3904: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3905;
    }
  goto ret0;

 L3905: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3906;
    }
  goto ret0;

 L3906: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 570;
    }
  goto ret0;

 L4133: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4134;
  goto ret0;

 L4134: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4135;
    }
  goto ret0;

 L4135: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4136;
    }
  goto ret0;

 L4136: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 589;
    }
  goto ret0;

 L5300: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L3621;
    }
  goto ret0;

 L3621: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5368;
  goto ret0;

 L5368: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3622;
    case MINUS:
      goto L3641;
    case MULT:
      goto L3660;
    case SMIN:
      goto L3704;
    case SMAX:
      goto L3713;
    case NEG:
      goto L3768;
    case FLOAT_TRUNCATE:
      goto L3850;
    default:
     break;
   }
  goto ret0;

 L3622: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3623;
    }
  goto ret0;

 L3623: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 543;
    }
  goto ret0;

 L3641: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3642;
    }
  goto ret0;

 L3642: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 545;
    }
  goto ret0;

 L3660: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3661;
    }
  goto ret0;

 L3661: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 547;
    }
  goto ret0;

 L3704: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3705;
    }
  goto ret0;

 L3705: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 552;
    }
  goto ret0;

 L3713: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3714;
    }
  goto ret0;

 L3714: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 553;
    }
  goto ret0;

 L3768: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L3769;
  goto ret0;

 L3769: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3770;
    }
  goto ret0;

 L3770: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 558;
    }
  goto ret0;

 L3850: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5375;
  goto ret0;

 L5375: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3851;
    case MINUS:
      goto L3883;
    case MULT:
      goto L3915;
    case NEG:
      goto L4145;
    default:
     break;
   }
  goto ret0;

 L3851: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3852;
    }
  goto ret0;

 L3852: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3853;
    }
  goto ret0;

 L3853: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 565;
    }
  goto ret0;

 L3883: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3884;
    }
  goto ret0;

 L3884: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3885;
    }
  goto ret0;

 L3885: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 568;
    }
  goto ret0;

 L3915: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3916;
    }
  goto ret0;

 L3916: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3917;
    }
  goto ret0;

 L3917: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 571;
    }
  goto ret0;

 L4145: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4146;
  goto ret0;

 L4146: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4147;
    }
  goto ret0;

 L4147: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4148;
    }
  goto ret0;

 L4148: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 590;
    }
  goto ret0;

 L5301: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L3828;
    }
  goto ret0;

 L3828: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5379;
  goto ret0;

 L5379: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3829;
    case MINUS:
      goto L3861;
    case MULT:
      goto L3893;
    case SMIN:
      goto L3994;
    case SMAX:
      goto L4004;
    case NEG:
      goto L4121;
    default:
     break;
   }
  goto ret0;

 L3829: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3830;
    }
  goto ret0;

 L3830: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L3831;
    }
  goto ret0;

 L3831: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 563;
    }
  goto ret0;

 L3861: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3862;
    }
  goto ret0;

 L3862: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L3863;
    }
  goto ret0;

 L3863: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 566;
    }
  goto ret0;

 L3893: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3894;
    }
  goto ret0;

 L3894: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L3895;
    }
  goto ret0;

 L3895: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 569;
    }
  goto ret0;

 L3994: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3995;
    }
  goto ret0;

 L3995: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L3996;
    }
  goto ret0;

 L3996: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 578;
    }
  goto ret0;

 L4004: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L4005;
    }
  goto ret0;

 L4005: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L4006;
    }
  goto ret0;

 L4006: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 579;
    }
  goto ret0;

 L4121: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L4122;
  goto ret0;

 L4122: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L4123;
    }
  goto ret0;

 L4123: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L4124;
    }
  goto ret0;

 L4124: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 588;
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

  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5388;
    case BImode:
      goto L5389;
    case SImode:
      goto L5390;
    case SFmode:
      goto L5392;
    case DFmode:
      goto L5393;
    case TFmode:
      goto L5394;
    default:
      break;
    }
  goto ret0;

 L5388: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L2863;
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2843;
    }
  goto ret0;

 L2863: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2864;
    }
  goto ret0;

 L2864: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2865;
    }
  goto ret0;

 L2865: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2866;
    }
  goto ret0;

 L2866: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2867;
    }
  goto ret0;

 L2867: ATTRIBUTE_UNUSED_LABEL
  if (((gr_register_operand (operands[3], DImode) && INTVAL (operands[1]) <= 16)
   || operands[3] == const0_rtx || operands[3] == constm1_rtx))
    {
      return 474;
    }
  goto ret0;

 L2843: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5395;
  goto ret0;

 L5395: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case SIGN_EXTRACT:
      goto L2844;
    case ZERO_EXTRACT:
      goto L2854;
    case AND:
      goto L2875;
    case PLUS:
      goto L4266;
    default:
     break;
   }
  goto ret0;

 L2844: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2845;
    }
  goto ret0;

 L2845: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2846;
    }
  goto ret0;

 L2846: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (const_int_operand (x3, DImode))
    {
      operands[3] = x3;
      return 472;
    }
  goto ret0;

 L2854: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2855;
    }
  goto ret0;

 L2855: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2856;
    }
  goto ret0;

 L2856: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (const_int_operand (x3, DImode))
    {
      operands[3] = x3;
      return 473;
    }
  goto ret0;

 L2875: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ASHIFT)
    goto L2876;
  goto ret0;

 L2876: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L2877;
    }
  goto ret0;

 L2877: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2878;
    }
  goto ret0;

 L2878: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2879;
    }
  goto ret0;

 L2879: ATTRIBUTE_UNUSED_LABEL
  if ((CONST_OK_FOR_M (INTVAL (operands[2]))
   && ia64_depz_field_mask (operands[3], operands[2]) > 0))
    {
      return 475;
    }
  goto ret0;

 L4266: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L4267;
  goto ret0;

 L4267: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4268;
    }
  goto ret0;

 L4268: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4269;
    }
  goto ret0;

 L4269: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[3] = x3;
      return 600;
    }
  goto ret0;

 L5389: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2994;
    }
  goto ret0;

 L2994: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode)
    goto L5399;
  goto ret0;

 L5399: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2995;
    case IOR:
      goto L3141;
    case EQ:
    case NE:
    case GT:
    case LE:
    case GTU:
    case LEU:
      goto L5401;
    case LT:
    case GE:
    case LTU:
    case GEU:
      goto L5402;
    default:
      goto L5403;
   }
 L5401: ATTRIBUTE_UNUSED_LABEL
  if (normal_comparison_operator (x2, BImode))
    {
      operands[1] = x2;
      goto L4380;
    }
 L5402: ATTRIBUTE_UNUSED_LABEL
  if (adjusted_comparison_operator (x2, BImode))
    {
      operands[1] = x2;
      goto L4389;
    }
 L5403: ATTRIBUTE_UNUSED_LABEL
  if (comparison_operator (x2, BImode))
    {
      operands[1] = x2;
      goto L4416;
    }
  goto ret0;

 L2995: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5405;
  goto ret0;

 L5405: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L3019;
    case NE:
      goto L3103;
    case EQ:
      goto L3117;
    case GE:
    case GT:
    case LE:
    case LT:
      goto L5404;
    default:
      goto ret0;
   }
 L5404: ATTRIBUTE_UNUSED_LABEL
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L2996;
    }
  goto ret0;

 L3019: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (signed_inequality_operator (x4, BImode))
    {
      operands[3] = x4;
      goto L3020;
    }
  goto ret0;

 L3020: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  switch (GET_MODE (x5))
    {
    case SImode:
      goto L5408;
    case DImode:
      goto L5409;
    default:
      break;
    }
  goto ret0;

 L5408: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L3021;
    }
  goto ret0;

 L3021: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0L)
    goto L3022;
  goto ret0;

 L3022: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 488;
    }
  goto ret0;

 L5409: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3067;
    }
  goto ret0;

 L3067: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0L)
    goto L3068;
  goto ret0;

 L3068: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 492;
    }
  goto ret0;

 L3103: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3104;
  goto ret0;

 L3104: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3105;
    }
  goto ret0;

 L3105: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3106;
  goto ret0;

 L3106: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3107;
    }
  goto ret0;

 L3107: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3108;
  goto ret0;

 L3108: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 495;
    }
  goto ret0;

 L3117: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3118;
  goto ret0;

 L3118: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3119;
    }
  goto ret0;

 L3119: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3120;
  goto ret0;

 L3120: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3121;
    }
  goto ret0;

 L3121: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3122;
  goto ret0;

 L3122: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 496;
    }
  goto ret0;

 L2996: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L5410;
    case DImode:
      goto L5411;
    default:
      break;
    }
  goto ret0;

 L5410: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L2997;
    }
  goto ret0;

 L2997: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2998;
  goto ret0;

 L2998: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 486;
    }
  goto ret0;

 L5411: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3043;
    }
  goto ret0;

 L3043: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3044;
  goto ret0;

 L3044: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 490;
    }
  goto ret0;

 L3141: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5413;
  goto ret0;

 L5413: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L3165;
    case NE:
      goto L3249;
    case EQ:
      goto L3263;
    case GE:
    case GT:
    case LE:
    case LT:
      goto L5412;
    default:
      goto ret0;
   }
 L5412: ATTRIBUTE_UNUSED_LABEL
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L3142;
    }
  goto ret0;

 L3165: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (signed_inequality_operator (x4, BImode))
    {
      operands[3] = x4;
      goto L3166;
    }
  goto ret0;

 L3166: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  switch (GET_MODE (x5))
    {
    case SImode:
      goto L5416;
    case DImode:
      goto L5417;
    default:
      break;
    }
  goto ret0;

 L5416: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L3167;
    }
  goto ret0;

 L3167: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0L)
    goto L3168;
  goto ret0;

 L3168: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 500;
    }
  goto ret0;

 L5417: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3213;
    }
  goto ret0;

 L3213: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0L)
    goto L3214;
  goto ret0;

 L3214: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 504;
    }
  goto ret0;

 L3249: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3250;
  goto ret0;

 L3250: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3251;
    }
  goto ret0;

 L3251: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3252;
  goto ret0;

 L3252: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3253;
    }
  goto ret0;

 L3253: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3254;
  goto ret0;

 L3254: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 507;
    }
  goto ret0;

 L3263: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3264;
  goto ret0;

 L3264: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3265;
    }
  goto ret0;

 L3265: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1L)
    goto L3266;
  goto ret0;

 L3266: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3267;
    }
  goto ret0;

 L3267: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3268;
  goto ret0;

 L3268: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 508;
    }
  goto ret0;

 L3142: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L5418;
    case DImode:
      goto L5419;
    default:
      break;
    }
  goto ret0;

 L5418: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3143;
    }
  goto ret0;

 L3143: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3144;
  goto ret0;

 L3144: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 498;
    }
  goto ret0;

 L5419: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3189;
    }
  goto ret0;

 L3189: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L3190;
  goto ret0;

 L3190: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 502;
    }
  goto ret0;

 L4380: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L4381;
    }
  if (gr_reg_or_0_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4399;
    }
  goto L5403;

 L4381: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[3] = x3;
      return 612;
    }
  goto L5403;

 L4399: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[3] = x3;
      return 614;
    }
  goto L5403;

 L4389: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L5420;
    case DImode:
      goto L5421;
    default:
      break;
    }
  goto L5403;

 L5420: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L4390;
    }
  goto L5403;

 L4390: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_adjusted_operand (x3, SImode))
    {
      operands[3] = x3;
      return 613;
    }
  goto L5403;

 L5421: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4408;
    }
  goto L5403;

 L4408: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_adjusted_operand (x3, DImode))
    {
      operands[3] = x3;
      return 615;
    }
  goto L5403;

 L4416: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5422;
    case DFmode:
      goto L5423;
    case TFmode:
      goto L5424;
    default:
      break;
    }
  goto ret0;

 L5422: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L4417;
    }
  goto ret0;

 L4417: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 616;
    }
  goto ret0;

 L5423: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L4426;
    }
  goto ret0;

 L4426: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 617;
    }
  goto ret0;

 L5424: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L4435;
    }
  goto ret0;

 L4435: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L4436;
    }
  goto ret0;

 L4436: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 618;
    }
  goto ret0;

 L5390: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3315;
    }
 L5391: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3354;
    }
  goto ret0;

 L3315: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L3316;
  x2 = XEXP (x1, 0);
  goto L5391;

 L3316: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L3317;
  x2 = XEXP (x1, 0);
  goto L5391;

 L3317: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3318;
    }
  x2 = XEXP (x1, 0);
  goto L5391;

 L3318: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3319;
    }
  x2 = XEXP (x1, 0);
  goto L5391;

 L3319: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      return 513;
    }
  x2 = XEXP (x1, 0);
  goto L5391;

 L3354: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L3355;
  goto ret0;

 L3355: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L3356;
  goto ret0;

 L3356: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (grfr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3357;
    }
  goto ret0;

 L3357: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (grfr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3358;
    }
  goto ret0;

 L3358: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      return 517;
    }
  goto ret0;

 L5392: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L3577;
    }
  goto ret0;

 L3577: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5425;
  goto ret0;

 L5425: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3578;
    case MINUS:
      goto L3589;
    case FLOAT_TRUNCATE:
      goto L3733;
    default:
     break;
   }
  goto ret0;

 L3578: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode)
    goto L5428;
  goto ret0;

 L5428: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L3579;
    case NEG:
      goto L3611;
    default:
     break;
   }
  goto ret0;

 L3579: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L3580;
    }
  goto ret0;

 L3580: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      goto L3581;
    }
  goto ret0;

 L3581: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 539;
    }
  goto ret0;

 L3611: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SFmode
      && GET_CODE (x4) == MULT)
    goto L3612;
  goto ret0;

 L3612: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, SFmode))
    {
      operands[1] = x5;
      goto L3613;
    }
  goto ret0;

 L3613: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, SFmode))
    {
      operands[2] = x5;
      goto L3614;
    }
  goto ret0;

 L3614: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 542;
    }
  goto ret0;

 L3589: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == MULT)
    goto L3590;
  goto ret0;

 L3590: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L3591;
    }
  goto ret0;

 L3591: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      goto L3592;
    }
  goto ret0;

 L3592: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 540;
    }
  goto ret0;

 L3733: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DFmode:
      goto L5430;
    case TFmode:
      goto L5432;
    default:
      break;
    }
  goto ret0;

 L5430: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3734;
    case MINUS:
      goto L3757;
    default:
     break;
   }
  goto ret0;

 L3734: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode)
    goto L5434;
  goto ret0;

 L5434: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L3735;
    case NEG:
      goto L3818;
    default:
     break;
   }
  goto ret0;

 L3735: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3736;
    }
  goto ret0;

 L3736: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L3737;
    }
  goto ret0;

 L3737: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      return 555;
    }
  goto ret0;

 L3818: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DFmode
      && GET_CODE (x5) == MULT)
    goto L3819;
  goto ret0;

 L3819: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DFmode))
    {
      operands[1] = x6;
      goto L3820;
    }
  goto ret0;

 L3820: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (fr_register_operand (x6, DFmode))
    {
      operands[2] = x6;
      goto L3821;
    }
  goto ret0;

 L3821: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      return 562;
    }
  goto ret0;

 L3757: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L3758;
  goto ret0;

 L3758: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3759;
    }
  goto ret0;

 L3759: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L3760;
    }
  goto ret0;

 L3760: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      return 557;
    }
  goto ret0;

 L5432: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L4027;
    case MINUS:
      goto L4096;
    default:
     break;
   }
  goto ret0;

 L4027: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5436;
  goto ret0;

 L5436: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L4028;
    case NEG:
      goto L4171;
    default:
     break;
   }
  goto ret0;

 L4028: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4029;
    }
  goto ret0;

 L4029: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4030;
    }
  goto ret0;

 L4030: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4031;
    }
  goto ret0;

 L4031: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 581;
    }
  goto ret0;

 L4171: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L4172;
  goto ret0;

 L4172: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L4173;
    }
  goto ret0;

 L4173: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L4174;
    }
  goto ret0;

 L4174: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4175;
    }
  goto ret0;

 L4175: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 592;
    }
  goto ret0;

 L4096: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4097;
  goto ret0;

 L4097: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4098;
    }
  goto ret0;

 L4098: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4099;
    }
  goto ret0;

 L4099: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4100;
    }
  goto ret0;

 L4100: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 586;
    }
  goto ret0;

 L5393: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L3721;
    }
  goto ret0;

 L3721: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5438;
  goto ret0;

 L5438: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3722;
    case MINUS:
      goto L3745;
    case FLOAT_TRUNCATE:
      goto L4039;
    default:
     break;
   }
  goto ret0;

 L3722: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L5441;
  goto ret0;

 L5441: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L3723;
    case NEG:
      goto L3790;
    default:
     break;
   }
  goto ret0;

 L3723: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3724;
    }
  goto ret0;

 L3724: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L3725;
    }
  goto ret0;

 L3725: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 554;
    }
  goto ret0;

 L3790: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L3791;
  goto ret0;

 L3791: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3792;
    }
  goto ret0;

 L3792: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L3793;
    }
  goto ret0;

 L3793: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 560;
    }
  goto ret0;

 L3745: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L3746;
  goto ret0;

 L3746: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3747;
    }
  goto ret0;

 L3747: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L3748;
    }
  goto ret0;

 L3748: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 556;
    }
  goto ret0;

 L4039: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5443;
  goto ret0;

 L5443: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L4040;
    case MINUS:
      goto L4109;
    default:
     break;
   }
  goto ret0;

 L4040: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5445;
  goto ret0;

 L5445: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L4041;
    case NEG:
      goto L4185;
    default:
     break;
   }
  goto ret0;

 L4041: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4042;
    }
  goto ret0;

 L4042: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4043;
    }
  goto ret0;

 L4043: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4044;
    }
  goto ret0;

 L4044: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 582;
    }
  goto ret0;

 L4185: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L4186;
  goto ret0;

 L4186: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L4187;
    }
  goto ret0;

 L4187: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L4188;
    }
  goto ret0;

 L4188: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4189;
    }
  goto ret0;

 L4189: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 593;
    }
  goto ret0;

 L4109: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4110;
  goto ret0;

 L4110: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4111;
    }
  goto ret0;

 L4111: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4112;
    }
  goto ret0;

 L4112: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4113;
    }
  goto ret0;

 L4113: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 587;
    }
  goto ret0;

 L5394: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L4013;
    }
  goto ret0;

 L4013: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5447;
  goto ret0;

 L5447: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L4014;
    case MINUS:
      goto L4083;
    default:
     break;
   }
  goto ret0;

 L4014: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5449;
  goto ret0;

 L5449: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L4015;
    case NEG:
      goto L4157;
    default:
     break;
   }
  goto ret0;

 L4015: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L4016;
    }
  goto ret0;

 L4016: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L4017;
    }
  goto ret0;

 L4017: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L4018;
    }
  goto ret0;

 L4018: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 580;
    }
  goto ret0;

 L4157: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4158;
  goto ret0;

 L4158: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4159;
    }
  goto ret0;

 L4159: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4160;
    }
  goto ret0;

 L4160: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L4161;
    }
  goto ret0;

 L4161: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 591;
    }
  goto ret0;

 L4083: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L4084;
  goto ret0;

 L4084: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L4085;
    }
  goto ret0;

 L4085: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L4086;
    }
  goto ret0;

 L4086: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L4087;
    }
  goto ret0;

 L4087: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 585;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_10 PARAMS ((rtx, rtx, int *));
static int
recog_10 (x0, insn, pnum_clobbers)
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
    case SET:
      goto L2982;
    case PARALLEL:
      goto L5483;
    default:
     break;
   }
  goto ret0;

 L2982: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case BImode:
      goto L5485;
    case DImode:
      goto L5486;
    default:
      break;
    }
  goto ret0;

 L5485: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2983;
    }
  goto ret0;

 L2983: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode)
    goto L5487;
  goto ret0;

 L5487: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2984;
    case IOR:
      goto L3130;
    default:
     break;
   }
  goto ret0;

 L2984: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5490;
  goto ret0;

 L5490: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == NOT)
    goto L3007;
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L3031;
    }
  goto ret0;

 L3007: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (predicate_operator (x4, BImode))
    {
      operands[4] = x4;
      goto L3054;
    }
  goto ret0;

 L3054: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3055;
    }
  if (gr_reg_or_0_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L3009;
    }
  goto ret0;

 L3055: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, DImode))
    {
      operands[3] = x5;
      goto L3056;
    }
  goto ret0;

 L3056: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 491;
    }
  goto ret0;

 L3009: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, SImode))
    {
      operands[3] = x5;
      goto L3010;
    }
  goto ret0;

 L3010: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 487;
    }
  goto ret0;

 L3031: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3032;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L2986;
    }
  goto ret0;

 L3032: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L3033;
    }
  goto ret0;

 L3033: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 489;
    }
  goto ret0;

 L2986: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L2987;
    }
  goto ret0;

 L2987: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 485;
    }
  goto ret0;

 L3130: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5492;
  goto ret0;

 L5492: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == NOT)
    goto L3153;
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L3177;
    }
  goto ret0;

 L3153: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (predicate_operator (x4, BImode))
    {
      operands[4] = x4;
      goto L3200;
    }
  goto ret0;

 L3200: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3201;
    }
  if (gr_reg_or_0_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L3155;
    }
  goto ret0;

 L3201: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, DImode))
    {
      operands[3] = x5;
      goto L3202;
    }
  goto ret0;

 L3202: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 503;
    }
  goto ret0;

 L3155: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, SImode))
    {
      operands[3] = x5;
      goto L3156;
    }
  goto ret0;

 L3156: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 499;
    }
  goto ret0;

 L3177: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3178;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3132;
    }
  goto ret0;

 L3178: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L3179;
    }
  goto ret0;

 L3179: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 501;
    }
  goto ret0;

 L3132: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L3133;
    }
  goto ret0;

 L3133: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 497;
    }
  goto ret0;

 L5486: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4276;
    }
  goto ret0;

 L4276: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L4277;
  goto ret0;

 L4277: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L4278;
  goto ret0;

 L4278: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L4279;
  goto ret0;

 L4279: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L4280;
    }
  goto ret0;

 L4280: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (shladd_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L4281;
    }
  goto ret0;

 L4281: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (nonmemory_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L4282;
    }
  goto ret0;

 L4282: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[4] = x3;
      goto L4283;
    }
  goto ret0;

 L4283: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 601;
    }
  goto ret0;

 L5483: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 2:
      goto L3431;
    case 4:
      goto L4531;
    default:
      break;
    }
  goto ret0;

 L3431: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L3432;
  goto ret0;

 L3432: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5493;
    case DFmode:
      goto L5494;
    case TFmode:
      goto L5495;
    default:
      break;
    }
  goto ret0;

 L5493: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L3433;
    }
  goto ret0;

 L3433: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L3434;
  goto ret0;

 L3434: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L3435;
  goto ret0;

 L3435: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (grfr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3436;
    }
  goto ret0;

 L3436: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (grfr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3437;
    }
  goto ret0;

 L3437: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (grfr_register_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L3438;
    }
  goto ret0;

 L3438: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L3439;
  goto ret0;

 L3439: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[4] = x3;
      return 525;
    }
  goto ret0;

 L5494: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[0] = x3;
      goto L3801;
    }
  goto ret0;

 L3801: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DFmode)
    goto L5496;
  goto ret0;

 L5496: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3802;
    case FLOAT_TRUNCATE:
      goto L4068;
    default:
     break;
   }
  goto ret0;

 L3802: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == NEG)
    goto L3803;
  goto ret0;

 L3803: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DFmode
      && GET_CODE (x5) == MULT)
    goto L3804;
  goto ret0;

 L3804: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DFmode))
    {
      operands[1] = x6;
      goto L3805;
    }
  goto ret0;

 L3805: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (fr_register_operand (x6, DFmode))
    {
      operands[2] = x6;
      goto L3806;
    }
  goto ret0;

 L3806: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      goto L3807;
    }
  goto ret0;

 L3807: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3808;
  goto ret0;

 L3808: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      return 561;
    }
  goto ret0;

 L4068: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == PLUS)
    goto L4069;
  goto ret0;

 L4069: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode)
    goto L5498;
  goto ret0;

 L5498: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x5))
    {
    case MULT:
      goto L4070;
    case NEG:
      goto L4216;
    default:
     break;
   }
  goto ret0;

 L4070: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L4071;
    }
  goto ret0;

 L4071: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L4072;
    }
  goto ret0;

 L4072: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[3] = x5;
      goto L4073;
    }
  goto ret0;

 L4073: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4074;
  goto ret0;

 L4074: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      goto L4075;
    }
  goto ret0;

 L4075: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 584;
    }
  goto ret0;

 L4216: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (GET_MODE (x6) == TFmode
      && GET_CODE (x6) == MULT)
    goto L4217;
  goto ret0;

 L4217: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 0);
  if (tfreg_or_fp01_operand (x7, TFmode))
    {
      operands[1] = x7;
      goto L4218;
    }
  goto ret0;

 L4218: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 1);
  if (tfreg_or_fp01_operand (x7, TFmode))
    {
      operands[2] = x7;
      goto L4219;
    }
  goto ret0;

 L4219: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[3] = x5;
      goto L4220;
    }
  goto ret0;

 L4220: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4221;
  goto ret0;

 L4221: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      goto L4222;
    }
  goto ret0;

 L4222: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 595;
    }
  goto ret0;

 L5495: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[0] = x3;
      goto L4052;
    }
  goto ret0;

 L4052: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == PLUS)
    goto L4053;
  goto ret0;

 L4053: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5500;
  goto ret0;

 L5500: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L4054;
    case NEG:
      goto L4199;
    default:
     break;
   }
  goto ret0;

 L4054: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4055;
    }
  goto ret0;

 L4055: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4056;
    }
  goto ret0;

 L4056: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4057;
    }
  goto ret0;

 L4057: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4058;
  goto ret0;

 L4058: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      goto L4059;
    }
  goto ret0;

 L4059: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 583;
    }
  goto ret0;

 L4199: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L4200;
  goto ret0;

 L4200: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L4201;
    }
  goto ret0;

 L4201: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L4202;
    }
  goto ret0;

 L4202: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4203;
    }
  goto ret0;

 L4203: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4204;
  goto ret0;

 L4204: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      goto L4205;
    }
  goto ret0;

 L4205: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 594;
    }
  goto ret0;

 L4531: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4532;
  goto ret0;

 L4532: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  goto L4533;

 L4533: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CALL)
    goto L4534;
  goto ret0;

 L4534: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MEM)
    goto L4535;
  goto ret0;

 L4535: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (call_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L4536;
    }
  goto ret0;

 L4536: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1L)
    goto L4537;
  goto ret0;

 L4537: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4538;
  goto ret0;

 L4538: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4539;
    }
  goto ret0;

 L4539: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == CLOBBER)
    goto L4540;
  goto ret0;

 L4540: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L4541;
    }
  goto ret0;

 L4541: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 3);
  if (GET_CODE (x2) == CLOBBER)
    goto L4542;
  goto ret0;

 L4542: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[4] = x3;
      return 627;
    }
  goto ret0;
 ret0:
  return -1;
}

static int recog_11 PARAMS ((rtx, rtx, int *));
static int
recog_11 (x0, insn, pnum_clobbers)
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
  if (predicate_operator (x1, VOIDmode))
    {
      operands[2] = x1;
      goto L2372;
    }
 L2394: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[3] = x1;
      goto L2395;
    }
 L2838: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[4] = x1;
      goto L2839;
    }
 L2906: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[2] = x1;
      goto L2907;
    }
 L2978: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[5] = x1;
      goto L2979;
    }
 L3441: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[6] = x1;
      goto L3442;
    }
 L4491: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[3] = x1;
      goto L4492;
    }
 L4504: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[1] = x1;
      goto L4505;
    }
 L4565: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L4566;
    }
 L4585: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[2] = x1;
      goto L4586;
    }
 L4596: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[4] = x1;
      goto L4597;
    }
 L4746: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[3] = x1;
      goto L4747;
    }
  goto ret0;

 L2372: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      goto L2373;
    }
  goto L2394;

 L2373: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L2374;
  goto L2394;

 L2374: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == SET)
    goto L2375;
  x1 = XEXP (x0, 0);
  goto L2394;

 L2375: ATTRIBUTE_UNUSED_LABEL
  tem = recog_7 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L2394;

 L2395: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[4] = x2;
      goto L2396;
    }
  goto L2838;

 L2396: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L2397;
  goto L2838;

 L2397: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5283;
    case SET:
      goto L2465;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2838;

 L5283: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 3:
      goto L2398;
    case 2:
      goto L2768;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2398: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L2399;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2399: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L5285;
    case DImode:
      goto L5286;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L5285: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L2400;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2400: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (symbolic_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L2401;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2401: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2402;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2402: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2403;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2403: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == USE)
    goto L2404;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2404: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    {
      return 423;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L5286: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2420;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2420: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2421;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2421: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2422;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2422: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2423;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2423: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == USE)
    goto L2424;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2424: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    {
      return 425;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2768: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L2769;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2769: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5288;
    case BImode:
      goto L5289;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L5288: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ZERO_EXTRACT)
    goto L2887;
  if (fr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2770;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2887: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L2888;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2888: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 32L)
    goto L2889;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2889: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2890;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2890: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2891;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2891: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2892;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2892: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 476;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2770: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5290;
  x1 = XEXP (x0, 0);
  goto L2838;

 L5290: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case FIX:
      goto L2771;
    case UNSIGNED_FIX:
      goto L2833;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2771: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2772;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2772: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L2773;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2773: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2774;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2774: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 464;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2833: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2834;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2834: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L2835;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2835: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2836;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2836: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 471;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L5289: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[0] = x3;
      goto L2973;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2973: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == BImode
      && GET_CODE (x3) == NOT)
    goto L2974;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2974: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2975;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2975: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2976;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2976: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, BImode))
    {
      operands[2] = x3;
      return 484;
    }
  x1 = XEXP (x0, 0);
  goto L2838;

 L2465: ATTRIBUTE_UNUSED_LABEL
  tem = recog_8 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L2838;

 L2839: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2840;
    }
  goto L2906;

 L2840: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L2841;
  goto L2906;

 L2841: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L2842;
    case PARALLEL:
      goto L5385;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2906;

 L2842: ATTRIBUTE_UNUSED_LABEL
  tem = recog_9 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L2906;

 L5385: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 2:
      goto L3923;
    case 4:
      goto L4516;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3923: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L3924;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3924: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case TFmode:
      goto L5451;
    case SFmode:
      goto L5452;
    case DFmode:
      goto L5453;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L5451: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[0] = x3;
      goto L3925;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3925: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L3926;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3926: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3927;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3927: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3928;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3928: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3929;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3929: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L3930;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3930: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 572;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L5452: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[0] = x3;
      goto L3938;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3938: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == FLOAT_TRUNCATE)
    goto L3939;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3939: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L3940;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3940: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3941;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3941: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3942;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3942: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3943;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3943: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L3944;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3944: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 573;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L5453: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[0] = x3;
      goto L3952;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3952: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == FLOAT_TRUNCATE)
    goto L3953;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3953: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L3954;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3954: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3955;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3955: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3956;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3956: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3957;
  x1 = XEXP (x0, 0);
  goto L2906;

 L3957: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L3958;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L3958: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 574;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L4516: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CALL)
    goto L4517;
  x1 = XEXP (x0, 0);
  goto L2906;

 L4517: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == MEM)
    goto L4518;
  x1 = XEXP (x0, 0);
  goto L2906;

 L4518: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, VOIDmode))
    {
      operands[0] = x4;
      goto L4519;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L4519: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    goto L4520;
  x1 = XEXP (x0, 0);
  goto L2906;

 L4520: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4521;
  x1 = XEXP (x0, 0);
  goto L2906;

 L4521: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4522;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L4522: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == CLOBBER)
    goto L4523;
  x1 = XEXP (x0, 0);
  goto L2906;

 L4523: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4524;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L4524: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 3);
  if (GET_CODE (x2) == CLOBBER)
    goto L4525;
  x1 = XEXP (x0, 0);
  goto L2906;

 L4525: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[3] = x3;
      return 626;
    }
  x1 = XEXP (x0, 0);
  goto L2906;

 L2907: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      goto L2908;
    }
  goto L2978;

 L2908: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L2909;
  goto L2978;

 L2909: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L2910;
    case PARALLEL:
      goto L5454;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2978;

 L2910: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5455;
    case SImode:
      goto L5456;
    case SFmode:
      goto L5458;
    case DFmode:
      goto L5459;
    case TFmode:
      goto L5460;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L5455: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L2911;
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L3393;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L2911: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2912;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L2912: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32L)
    goto L2913;
  x1 = XEXP (x0, 0);
  goto L2978;

 L2913: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32L)
    goto L2914;
  x1 = XEXP (x0, 0);
  goto L2978;

 L2914: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_0_operand (x2, DImode))
    {
      operands[1] = x2;
      return 478;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3393: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5461;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5461: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3394;
    case NEG:
      goto L3492;
    case UNSPEC:
      goto L5467;
    case NOT:
      goto L4372;
    case NE:
      goto L4468;
    case EQ:
      goto L4477;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3394: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L3395;
  x1 = XEXP (x0, 0);
  goto L2978;

 L3395: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L3396;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3396: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 2L)
    goto L3397;
  x1 = XEXP (x0, 0);
  goto L2978;

 L3397: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    {
      return 521;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3492: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 529;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L5467: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 16)
    goto L3500;
  x1 = XEXP (x0, 0);
  goto L2978;

 L3500: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 530;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L4372: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 611;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L4468: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L4469;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L4469: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    {
      return 621;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L4477: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L4478;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L4478: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    {
      return 622;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L5456: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3304;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3304: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L5468;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5468: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3305;
    case NEG:
      goto L3366;
    case NOT:
      goto L4327;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3305: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L3306;
  x1 = XEXP (x0, 0);
  goto L2978;

 L3306: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3307;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3307: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 2L)
    goto L3308;
  x1 = XEXP (x0, 0);
  goto L2978;

 L3308: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    {
      return 512;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3366: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 518;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L4327: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 606;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L5458: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L3534;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3534: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5471;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5471: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L3535;
    case NEG:
      goto L3543;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3535: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 534;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3543: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode)
    goto L5474;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5474: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ABS)
    goto L3552;
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 535;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3552: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      return 536;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L5459: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L3678;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3678: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5475;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5475: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L3679;
    case NEG:
      goto L3687;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3679: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 549;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3687: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L5478;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5478: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ABS)
    goto L3696;
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 550;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3696: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      return 551;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L5460: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L3965;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3965: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5479;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5479: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L3966;
    case NEG:
      goto L3975;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3966: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3967;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3967: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 575;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3975: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5482;
  x1 = XEXP (x0, 0);
  goto L2978;

 L5482: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ABS)
    goto L3985;
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3976;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3985: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3986;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3986: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 577;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L3976: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT))
    {
      return 576;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L5454: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4484;
  x1 = XEXP (x0, 0);
  goto L2978;

 L4484: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CALL)
    goto L4485;
  x1 = XEXP (x0, 0);
  goto L2978;

 L4485: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L4486;
  x1 = XEXP (x0, 0);
  goto L2978;

 L4486: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L4487;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L4487: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L4488;
  x1 = XEXP (x0, 0);
  goto L2978;

 L4488: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4489;
  x1 = XEXP (x0, 0);
  goto L2978;

 L4489: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 623;
    }
  x1 = XEXP (x0, 0);
  goto L2978;

 L2979: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2980;
    }
  goto L3441;

 L2980: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L2981;
  goto L3441;

 L2981: ATTRIBUTE_UNUSED_LABEL
  tem = recog_10 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L3441;

 L3442: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[7] = x2;
      goto L3443;
    }
  goto L4491;

 L3443: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L3444;
  goto L4491;

 L3444: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == PARALLEL
      && XVECLEN (x1, 0) == 2)
    goto L3445;
  x1 = XEXP (x0, 0);
  goto L4491;

 L3445: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L3446;
  x1 = XEXP (x0, 0);
  goto L4491;

 L3446: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L3447;
    }
  x1 = XEXP (x0, 0);
  goto L4491;

 L3447: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L3448;
  x1 = XEXP (x0, 0);
  goto L4491;

 L3448: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == PLUS)
    goto L3449;
  x1 = XEXP (x0, 0);
  goto L4491;

 L3449: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DImode
      && GET_CODE (x5) == MULT)
    goto L3450;
  x1 = XEXP (x0, 0);
  goto L4491;

 L3450: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L3451;
    }
  x1 = XEXP (x0, 0);
  goto L4491;

 L3451: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L3452;
    }
  x1 = XEXP (x0, 0);
  goto L4491;

 L3452: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (register_operand (x5, DImode))
    {
      operands[3] = x5;
      goto L3453;
    }
  x1 = XEXP (x0, 0);
  goto L4491;

 L3453: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (nonmemory_operand (x4, DImode))
    {
      operands[4] = x4;
      goto L3454;
    }
  x1 = XEXP (x0, 0);
  goto L4491;

 L3454: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L3455;
  x1 = XEXP (x0, 0);
  goto L4491;

 L3455: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[5] = x3;
      goto L3456;
    }
  x1 = XEXP (x0, 0);
  goto L4491;

 L3456: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 526;
    }
  x1 = XEXP (x0, 0);
  goto L4491;

 L4492: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[4] = x2;
      goto L4493;
    }
  goto L4504;

 L4493: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L4494;
  goto L4504;

 L4494: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5502;
    case PREFETCH:
      goto L4715;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4504;

 L5502: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 2:
      goto L4495;
    case 3:
      goto L4548;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4495: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4496;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4496: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  goto L4497;
 L4738: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L4739;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4497: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CALL)
    goto L4498;
  x3 = XEXP (x2, 0);
  goto L4738;

 L4498: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MEM)
    goto L4499;
  x3 = XEXP (x2, 0);
  goto L4738;

 L4499: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (call_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L4500;
    }
  x3 = XEXP (x2, 0);
  goto L4738;

 L4500: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L4501;
  x3 = XEXP (x2, 0);
  goto L4738;

 L4501: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4502;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L4738;

 L4502: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 624;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L4738;

 L4739: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    goto L4740;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4740: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4741;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4741: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4742;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4742: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 20)
    goto L4743;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4743: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4744;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4744: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (fetchadd_operand (x4, SImode))
    {
      operands[2] = x4;
      return 652;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4548: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CALL)
    goto L4549;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4549: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L4550;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4550: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L4551;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4551: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    goto L4552;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4552: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4553;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4553: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4554;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4554: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == CLOBBER)
    goto L4555;
  x1 = XEXP (x0, 0);
  goto L4504;

 L4555: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      return 628;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4715: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (address_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4716;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4716: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L4717;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4717: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      return 649;
    }
  x1 = XEXP (x0, 0);
  goto L4504;

 L4505: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L4506;
    }
  goto L4565;

 L4506: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L4507;
  goto L4565;

 L4507: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case CALL:
      goto L4508;
    case PARALLEL:
      goto L5504;
    case SET:
      goto L4664;
    case TRAP_IF:
      goto L4702;
    case UNSPEC_VOLATILE:
      goto L5505;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4565;

 L4508: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L4509;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4509: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4510;
    }
  x1 = XEXP (x0, 0);
  goto L4565;

 L4510: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return 625;
    }
  x1 = XEXP (x0, 0);
  goto L4565;

 L5504: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4561;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4561: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == RETURN)
    goto L4562;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4562: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4563;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4563: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      return 629;
    }
  x1 = XEXP (x0, 0);
  goto L4565;

 L4664: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4665;
    }
  if (GET_CODE (x2) == PC)
    goto L4583;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4665: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 21)
    goto L4666;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4666: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    {
      return 640;
    }
  x1 = XEXP (x0, 0);
  goto L4565;

 L4583: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      return 632;
    }
  if (GET_CODE (x2) == LABEL_REF)
    goto L4576;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4576: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  return 631;

 L4702: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    goto L4703;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4703: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, VOIDmode))
    {
      operands[0] = x2;
      return 647;
    }
  x1 = XEXP (x0, 0);
  goto L4565;

 L5505: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 7)
    goto L4723;
  x1 = XEXP (x0, 0);
  goto L4565;

 L4723: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  operands[0] = x2;
  return 650;

 L4566: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L4567;
    }
  goto L4585;

 L4567: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L4568;
  goto L4585;

 L4568: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case RETURN:
      goto L5506;
    case CONST_INT:
      goto L5507;
    case UNSPEC_VOLATILE:
      goto L5513;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4585;

 L5506: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_direct_return ()))
    {
      return 630;
    }
  x1 = XEXP (x0, 0);
  goto L4585;

 L5507: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x1, 0) == XWINT (x1, 0))
    switch ((int) XWINT (x1, 0))
      {
      case 0L:
        goto L5514;
      case 1L:
        goto L5515;
      case 2L:
        goto L5516;
      case 3L:
        goto L5517;
      case 4L:
        goto L5518;
      case 5L:
        goto L5519;
      default:
        break;
      }
  x1 = XEXP (x0, 0);
  goto L4585;

 L5514: ATTRIBUTE_UNUSED_LABEL
  return 641;

 L5515: ATTRIBUTE_UNUSED_LABEL
  return 642;

 L5516: ATTRIBUTE_UNUSED_LABEL
  return 643;

 L5517: ATTRIBUTE_UNUSED_LABEL
  return 644;

 L5518: ATTRIBUTE_UNUSED_LABEL
  return 645;

 L5519: ATTRIBUTE_UNUSED_LABEL
  return 646;

 L5513: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 3)
    goto L4709;
  x1 = XEXP (x0, 0);
  goto L4585;

 L4709: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return 648;
    }
  x1 = XEXP (x0, 0);
  goto L4585;

 L4586: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      goto L4587;
    }
  goto L4596;

 L4587: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L4588;
  goto L4596;

 L4588: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5520;
    case SET:
      goto L4648;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4596;

 L5520: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4589;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4589: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4614;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4614: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4615;
    }
  if (GET_CODE (x3) == PC)
    goto L4591;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4615: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4616;
    }
  x1 = XEXP (x0, 0);
  goto L4596;

 L4616: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4617;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4617: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L4618;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4618: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    {
      return 635;
    }
  x1 = XEXP (x0, 0);
  goto L4596;

 L4591: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4592;
    }
  x1 = XEXP (x0, 0);
  goto L4596;

 L4592: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4593;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4593: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == LABEL_REF)
    goto L4594;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4594: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  operands[1] = x4;
  return 633;

 L4648: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5521;
 L4729: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L4730;
 L4813: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4814;
    }
  x1 = XEXP (x0, 0);
  goto L4596;

 L5521: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L4649;
    }
 L5522: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L4657;
    }
  goto L4729;

 L4649: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 12)
    goto L4650;
  x2 = XEXP (x1, 0);
  goto L5522;

 L4650: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (register_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 638;
    }
  x2 = XEXP (x1, 0);
  goto L5522;

 L4657: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 13)
    goto L4658;
  x2 = XEXP (x1, 0);
  goto L4729;

 L4658: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (memory_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 639;
    }
  x2 = XEXP (x1, 0);
  goto L4729;

 L4730: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BLKmode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 18)
    goto L4731;
  x2 = XEXP (x1, 0);
  goto L4813;

 L4731: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  operands[1] = x3;
  return 651;

 L4814: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 24)
    goto L4815;
  x1 = XEXP (x0, 0);
  goto L4596;

 L4815: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 658;
    }
  x1 = XEXP (x0, 0);
  goto L4596;

 L4597: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L4598;
    }
  goto L4746;

 L4598: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L4599;
  goto L4746;

 L4599: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == PARALLEL
      && XVECLEN (x1, 0) == 2)
    goto L4600;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4600: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4601;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4601: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5523;
    case SImode:
      goto L5525;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L5523: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4602;
    }
 L5524: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4626;
    }
 L5526: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4779;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L4602: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5527;
  x3 = XEXP (x2, 0);
  goto L5524;

 L5527: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L4603;
    case UNSPEC:
      goto L5529;
    default:
     break;
   }
  x3 = XEXP (x2, 0);
  goto L5524;

 L4603: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4604;
    }
  x3 = XEXP (x2, 0);
  goto L5524;

 L4604: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_22bit_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4605;
    }
  x3 = XEXP (x2, 0);
  goto L5524;

 L4605: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4606;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5524;

 L4606: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L4607;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5524;

 L4607: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[3]))
    {
      return 634;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5524;

 L5529: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 11)
    goto L4639;
  x3 = XEXP (x2, 0);
  goto L5524;

 L4639: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (memory_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4640;
    }
  x3 = XEXP (x2, 0);
  goto L5524;

 L4640: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4641;
    }
  x3 = XEXP (x2, 0);
  goto L5524;

 L4641: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4642;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5524;

 L4642: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      return 637;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5524;

 L4626: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 10)
    goto L4627;
  x3 = XEXP (x2, 0);
  goto L5526;

 L4627: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4628;
    }
  x3 = XEXP (x2, 0);
  goto L5526;

 L4628: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4629;
    }
  x3 = XEXP (x2, 0);
  goto L5526;

 L4629: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4630;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5526;

 L4630: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      return 636;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5526;

 L4779: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    goto L4780;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4780: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4781;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4781: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4782;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L4782: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 3
      && XINT (x3, 1) == 19)
    goto L4783;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4783: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4784;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4784: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4785;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L4785: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 2);
  if (ar_ccv_reg_operand (x4, DImode))
    {
      operands[3] = x4;
      return 655;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L5525: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L4765;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L4765: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    goto L4766;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4766: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4767;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4767: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4768;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L4768: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 3
      && XINT (x3, 1) == 19)
    goto L4769;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4769: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4770;
  x1 = XEXP (x0, 0);
  goto L4746;

 L4770: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L4771;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L4771: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 2);
  if (ar_ccv_reg_operand (x4, VOIDmode))
    {
      operands[3] = x4;
      return 654;
    }
  x1 = XEXP (x0, 0);
  goto L4746;

 L4747: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[4] = x2;
      goto L4748;
    }
  goto ret0;

 L4748: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L4749;
  goto ret0;

 L4749: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5530;
    case SET:
      goto L4821;
    default:
     break;
   }
  goto ret0;

 L5530: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4750;
  goto ret0;

 L4750: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4751;
  goto ret0;

 L4751: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5531;
    case SImode:
      goto L5532;
    default:
      break;
    }
  goto ret0;

 L5531: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4804;
    }
  goto ret0;

 L4804: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (not_postinc_memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4805;
    }
 L4752: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x3, operands[1]))
    goto L4753;
  goto ret0;

 L4805: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4806;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  goto L4752;

 L4806: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L4807;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  goto L4752;

 L4807: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 657;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  goto L4752;

 L4753: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4754;
  goto ret0;

 L4754: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4755;
    }
  goto ret0;

 L4755: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 20)
    goto L4756;
  goto ret0;

 L4756: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4757;
  goto ret0;

 L4757: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (fetchadd_operand (x4, DImode))
    {
      operands[2] = x4;
      return 653;
    }
  goto ret0;

 L5532: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L4793;
    }
  goto ret0;

 L4793: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (not_postinc_memory_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4794;
    }
  goto ret0;

 L4794: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4795;
  goto ret0;

 L4795: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L4796;
  goto ret0;

 L4796: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 656;
    }
  goto ret0;

 L4821: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4822;
    }
  goto ret0;

 L4822: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 24)
    goto L4823;
  goto ret0;

 L4823: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == PLUS)
    goto L4824;
  goto ret0;

 L4824: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SImode)
    goto L5533;
  goto ret0;

 L5533: ATTRIBUTE_UNUSED_LABEL
  if (basereg_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L4825;
    }
 L5534: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L4836;
    }
  goto ret0;

 L4825: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_14bit_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L4826;
    }
  x4 = XEXP (x3, 0);
  goto L5534;

 L4826: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 659;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  x3 = XVECEXP (x2, 0, 0);
  x4 = XEXP (x3, 0);
  goto L5534;

 L4836: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (basereg_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L4837;
    }
  goto ret0;

 L4837: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 660;
    }
  goto ret0;
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

  if (GET_MODE (x0) == DImode
      && GET_CODE (x0) == UNSPEC
      && XVECLEN (x0, 0) == 1
      && XINT (x0, 1) == 25)
    goto L1752;
  switch (GET_CODE (x0))
    {
    case SET:
      goto L1;
    case PARALLEL:
      goto L4838;
    case CALL:
      goto L1642;
    case RETURN:
      goto L4843;
    case UNSPEC_VOLATILE:
      goto L4844;
    case UNSPEC:
      goto L4845;
    case CONST_INT:
      goto L4846;
    case TRAP_IF:
      goto L1811;
    case PREFETCH:
      goto L1821;
    case COND_EXEC:
      goto L2371;
    default:
      goto ret0;
   }
 L1796: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x0))
    {
    case UNSPEC:
      goto L4845;
    case CONST_INT:
      goto L4846;
    case TRAP_IF:
      goto L1811;
    case PREFETCH:
      goto L1821;
    case COND_EXEC:
      goto L2371;
    default:
     break;
   }
  goto ret0;

 L1752: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      return 240;
    }
  goto L1796;

 L1: ATTRIBUTE_UNUSED_LABEL
  return recog_4 (x0, insn, pnum_clobbers);

 L4838: ATTRIBUTE_UNUSED_LABEL
  return recog_6 (x0, insn, pnum_clobbers);

 L1642: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == MEM)
    goto L1643;
  goto ret0;

 L1643: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (call_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1644;
    }
  goto ret0;

 L1644: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == CONST_INT)
    goto L5197;
  goto ret0;

 L5197: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x1, 0) == XWINT (x1, 0))
    switch ((int) XWINT (x1, 0))
      {
      case 0L:
        goto L5199;
      case 1L:
        goto L5200;
      default:
        break;
      }
  goto ret0;

 L5199: ATTRIBUTE_UNUSED_LABEL
  return 227;

 L5200: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 230;
    }
  goto ret0;

 L4843: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_direct_return ()))
    {
      return 232;
    }
  goto ret0;

 L4844: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1)
    goto L5201;
  goto ret0;

 L5201: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x0, 1))
    {
    case 4L:
      goto L1795;
    case 1L:
      goto L1807;
    case 2L:
      goto L1809;
    case 3L:
      goto L1819;
    case 7L:
      goto L1825;
    case 5L:
      goto L1887;
    case 6L:
      goto L1889;
    default:
      break;
    }
  goto ret0;

 L1795: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      return 247;
    }
  goto ret0;

 L1807: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0L)
    {
      return 256;
    }
  goto ret0;

 L1809: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      return 257;
    }
  goto ret0;

 L1819: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0L)
    {
      return 260;
    }
  goto ret0;

 L1825: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  operands[0] = x1;
  return 262;

 L1887: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0L)
    {
      return 271;
    }
  goto ret0;

 L1889: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0L)
    {
      return 272;
    }
  goto ret0;

 L4845: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1)
    goto L5208;
  goto ret0;

 L5208: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x0, 1))
    {
    case 22L:
      goto L1797;
    case 23L:
      goto L1805;
    default:
      break;
    }
  goto ret0;

 L1797: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0L)
    {
      return 248;
    }
  goto ret0;

 L1805: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      return 255;
    }
  goto ret0;

 L4846: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x0, 0) == XWINT (x0, 0))
    switch ((int) XWINT (x0, 0))
      {
      case 0L:
        goto L5210;
      case 1L:
        goto L5211;
      case 2L:
        goto L5212;
      case 3L:
        goto L5213;
      case 4L:
        goto L5214;
      case 5L:
        goto L5215;
      default:
        break;
      }
  goto ret0;

 L5210: ATTRIBUTE_UNUSED_LABEL
  return 249;

 L5211: ATTRIBUTE_UNUSED_LABEL
  return 250;

 L5212: ATTRIBUTE_UNUSED_LABEL
  return 251;

 L5213: ATTRIBUTE_UNUSED_LABEL
  return 252;

 L5214: ATTRIBUTE_UNUSED_LABEL
  return 253;

 L5215: ATTRIBUTE_UNUSED_LABEL
  return 254;

 L1811: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 1L)
    goto L1812;
  if (predicate_operator (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L1815;
    }
  goto ret0;

 L1812: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      return 258;
    }
  goto ret0;

 L1815: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1816;
    }
  goto ret0;

 L1816: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    goto L1817;
  goto ret0;

 L1817: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[2] = x1;
      return 259;
    }
  goto ret0;

 L1821: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (address_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1822;
    }
  goto ret0;

 L1822: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L1823;
    }
  goto ret0;

 L1823: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 2);
  if (const_int_operand (x1, DImode))
    {
      operands[2] = x1;
      return 261;
    }
  goto ret0;

 L2371: ATTRIBUTE_UNUSED_LABEL
  return recog_11 (x0, insn, pnum_clobbers);
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
    case BImode:
      goto L5541;
    case DImode:
      goto L5542;
    case TImode:
      goto L5543;
    case SImode:
      goto L5544;
    default:
      break;
    }
 L2235: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L2236;
    }
  goto ret0;

 L5541: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L1910;
    }
  goto L2235;

 L1910: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BImode)
    goto L5548;
  x1 = XEXP (x0, 0);
  goto L2235;

 L5548: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case AND:
      goto L1970;
    case IOR:
      goto L1983;
    case NE:
      goto L2012;
    case EQ:
      goto L2021;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5547;
    default:
      x1 = XEXP (x0, 0);
      goto L2235;
   }
 L5547: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[1] = x1;
      goto L1911;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1970: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L5553;
  x1 = XEXP (x0, 0);
  goto L2235;

 L5553: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L1977;
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1971;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1977: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1978;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1978: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1979;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1979: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_304 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1971: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1972;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1972: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[2]) == REG && PR_REGNO_P (REGNO (operands[2]))))
    {
      return gen_split_303 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1983: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L5555;
  x1 = XEXP (x0, 0);
  goto L2235;

 L5555: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L1990;
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1984;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1990: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1991;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1991: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1992;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1992: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_306 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1984: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1985;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1985: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[2]) == REG && PR_REGNO_P (REGNO (operands[2]))))
    {
      return gen_split_305 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2012: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L5556;
  x1 = XEXP (x0, 0);
  goto L2235;

 L5556: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2013;
    case IOR:
      goto L2031;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2013: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L2014;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2014: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L2015;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2015: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2016;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2016: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2017;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2017: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return gen_split_309 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2031: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L2032;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2032: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L2033;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2033: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2034;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2034: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2035;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2035: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return gen_split_311 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2021: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L5558;
  x1 = XEXP (x0, 0);
  goto L2235;

 L5558: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2022;
    case IOR:
      goto L2040;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2022: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L2023;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2023: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L2024;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2024: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2025;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2025: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2026;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2026: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return gen_split_310 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2040: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L2041;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2041: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L2042;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2042: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2043;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2043: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2044;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2044: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L)
    {
      return gen_split_312 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1911: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && GR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_276 (operands);
    }
 L1915: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_277 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L5542: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1934;
    }
 L5545: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2216;
    }
  goto L2235;

 L1934: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L5561;
  x1 = XEXP (x0, 0);
  goto L5545;

 L5561: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == IF_THEN_ELSE)
    goto L2246;
  if (symbolic_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L1935;
    }
  x1 = XEXP (x0, 0);
  goto L5545;

 L2246: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2247;
    }
  x1 = XEXP (x0, 0);
  goto L5545;

 L2247: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2248;
    }
  x1 = XEXP (x0, 0);
  goto L5545;

 L2248: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L2249;
  x1 = XEXP (x0, 0);
  goto L5545;

 L2249: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == NEG)
    goto L2250;
  x1 = XEXP (x0, 0);
  goto L5545;

 L2250: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2251;
    }
  x1 = XEXP (x0, 0);
  goto L5545;

 L2251: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2252;
    }
  x1 = XEXP (x0, 0);
  goto L5545;

 L2252: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && rtx_equal_p (operands[0], operands[3])))
    {
      return gen_split_390 (operands);
    }
 L2262: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_391 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5545;

 L1935: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && ! TARGET_NO_PIC))
    {
      return gen_split_284 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5545;

 L2216: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L5562;
  x1 = XEXP (x0, 0);
  goto L2235;

 L5562: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L2217;
    case NE:
      goto L2227;
    case EQ:
      goto L2232;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2217: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L2218;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2218: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L2219;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2219: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L2220;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2220: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2221;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2221: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2222;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2222: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L2223;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2223: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_353 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2227: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L2228;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2228: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L
      && (reload_completed))
    {
      return gen_split_387 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2232: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L2233;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2233: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0L
      && (reload_completed))
    {
      return gen_split_388 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L5543: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, TImode))
    {
      operands[0] = x1;
      goto L1945;
    }
  goto L2235;

 L1945: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, TImode))
    {
      operands[1] = x1;
      goto L1946;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L1946: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_294 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L5544: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2204;
    }
 L5546: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2265;
    }
  goto L2235;

 L2204: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L5565;
  x1 = XEXP (x0, 0);
  goto L5546;

 L5565: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case ROTATERT:
      goto L2205;
    case ROTATE:
      goto L2211;
    case PLUS:
    case MINUS:
    case IOR:
    case XOR:
    case AND:
      goto L5567;
    default:
      x1 = XEXP (x0, 0);
      goto L5546;
   }
 L5567: ATTRIBUTE_UNUSED_LABEL
  if (condop_operator (x1, SImode))
    {
      operands[5] = x1;
      goto L2286;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2205: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2206;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2206: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_5bit_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2207;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2207: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_350 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2211: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2212;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2212: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_32bit_count_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2213;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2213: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_352 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2286: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L5568;
  x1 = XEXP (x0, 0);
  goto L5546;

 L5568: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == IF_THEN_ELSE)
    goto L2287;
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L2298;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2287: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L2288;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2288: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2289;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2289: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2290;
  x1 = XEXP (x0, 0);
  goto L5546;

 L2290: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2291;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2291: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L2292;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2292: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L2293;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2293: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_394 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2298: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == IF_THEN_ELSE)
    goto L2299;
  x1 = XEXP (x0, 0);
  goto L5546;

 L2299: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L2300;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2300: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2301;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2301: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0L)
    goto L2302;
  x1 = XEXP (x0, 0);
  goto L5546;

 L2302: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2303;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2303: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L2304;
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2304: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_395 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5546;

 L2265: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == IF_THEN_ELSE)
    goto L2266;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2266: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2267;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2267: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2268;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2268: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L2269;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2269: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == NEG)
    goto L2270;
  x1 = XEXP (x0, 0);
  goto L2235;

 L2270: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2271;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2271: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2272;
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2272: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && rtx_equal_p (operands[0], operands[3])))
    {
      return gen_split_392 (operands);
    }
 L2282: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_393 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2235;

 L2236: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == IF_THEN_ELSE)
    goto L2237;
  goto ret0;

 L2237: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2238;
    }
  goto ret0;

 L2238: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2239;
    }
  goto ret0;

 L2239: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L2240;
  goto ret0;

 L2240: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, VOIDmode))
    {
      operands[2] = x2;
      goto L2241;
    }
  goto ret0;

 L2241: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (move_operand (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2242;
    }
  goto ret0;

 L2242: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_389 (operands);
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
    case 3:
      goto L1917;
    case 2:
      goto L1937;
    case 5:
      goto L2060;
    case 4:
      goto L2105;
    case 6:
      goto L2173;
    default:
      break;
    }
  goto ret0;

 L1917: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L1918;
    case CALL:
      goto L2359;
    default:
     break;
   }
  goto ret0;

 L1918: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L5570;
    case DImode:
      goto L5571;
    default:
      break;
    }
  goto ret0;

 L5570: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L1919;
    }
  goto ret0;

 L1919: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (symbolic_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1920;
    }
  goto ret0;

 L1920: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1921;
  goto ret0;

 L1921: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1922;
    }
  goto ret0;

 L1922: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1923;
  goto ret0;

 L1923: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1
      && (!no_new_pseudos || reload_completed))
    {
      return gen_split_281 (operands);
    }
  goto ret0;

 L5571: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1927;
    }
  goto ret0;

 L1927: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1928;
    }
  goto ret0;

 L1928: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1929;
  goto ret0;

 L1929: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1930;
    }
  goto ret0;

 L1930: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1931;
  goto ret0;

 L1931: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1
      && (!no_new_pseudos || reload_completed))
    {
      return gen_split_283 (operands);
    }
  goto ret0;

 L2359: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L2360;
  goto ret0;

 L2360: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2361;
    }
  goto ret0;

 L2361: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    goto L2362;
  goto ret0;

 L2362: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2363;
  goto ret0;

 L2363: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2364;
    }
  goto ret0;

 L2364: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2365;
  goto ret0;

 L2365: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2366;
    }
  goto ret0;

 L2366: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_405 (operands);
    }
  goto ret0;

 L1937: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L1938;
  goto ret0;

 L1938: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TImode:
      goto L5572;
    case DImode:
      goto L5573;
    case BImode:
      goto L5574;
    default:
      break;
    }
  goto ret0;

 L5572: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, TImode))
    {
      operands[0] = x2;
      goto L1939;
    }
  goto ret0;

 L1939: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, TImode))
    {
      operands[1] = x2;
      goto L1940;
    }
  goto ret0;

 L1940: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1941;
  goto ret0;

 L1941: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1942;
    }
  goto ret0;

 L1942: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_293 (operands);
    }
  goto ret0;

 L5573: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L1950;
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2078;
    }
  goto ret0;

 L1950: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L1951;
    }
  goto ret0;

 L1951: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32L)
    goto L1952;
  goto ret0;

 L1952: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0L)
    goto L1953;
  goto ret0;

 L1953: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1954;
    }
  goto ret0;

 L1954: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1955;
  goto ret0;

 L1955: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1956;
    }
  goto ret0;

 L1956: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_301 (operands);
    }
 L1966: ATTRIBUTE_UNUSED_LABEL
  if ((! reload_completed))
    {
      return gen_split_302 (operands);
    }
  goto ret0;

 L2078: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L2079;
  goto ret0;

 L2079: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L2080;
  goto ret0;

 L2080: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L2081;
  goto ret0;

 L2081: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L2082;
    }
  goto ret0;

 L2082: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L2083;
    }
  goto ret0;

 L2083: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L2084;
    }
  goto ret0;

 L2084: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_14bit_operand (x3, DImode))
    {
      operands[4] = x3;
      goto L2085;
    }
  goto ret0;

 L2085: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2086;
  goto ret0;

 L2086: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[5] = x2;
      goto L2087;
    }
  goto ret0;

 L2087: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_324 (operands);
    }
  goto ret0;

 L5574: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L1996;
    }
  goto ret0;

 L1996: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == NOT)
    goto L1997;
  goto ret0;

 L1997: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1998;
    }
  goto ret0;

 L1998: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1999;
  goto ret0;

 L1999: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L2000;
    }
  goto ret0;

 L2000: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && rtx_equal_p (operands[0], operands[1])))
    {
      return gen_split_307 (operands);
    }
 L2008: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))
   && ! rtx_equal_p (operands[0], operands[1])))
    {
      return gen_split_308 (operands);
    }
  goto ret0;

 L2060: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L2061;
  goto ret0;

 L2061: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5576;
    case DFmode:
      goto L5577;
    default:
      break;
    }
  goto ret0;

 L5576: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2062;
    }
  goto ret0;

 L2062: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == FLOAT)
    goto L2063;
  goto ret0;

 L2063: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L2064;
  goto ret0;

 L2064: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2065;
    }
  goto ret0;

 L2065: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L2066;
    }
  goto ret0;

 L2066: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2067;
  goto ret0;

 L2067: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5578;
  goto ret0;

 L5578: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2068;
    }
 L5579: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2097;
    }
  goto ret0;

 L2068: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2069;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5579;

 L2069: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L2070;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5579;

 L2070: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2071;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5579;

 L2071: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2072;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5579;

 L2072: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == USE)
    goto L2073;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5579;

 L2073: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2074;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5579;

 L2074: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV&& reload_completed))
    {
      return gen_split_323 (operands);
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5579;

 L2097: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2098;
  goto ret0;

 L2098: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2099;
    }
  goto ret0;

 L2099: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2100;
  goto ret0;

 L2100: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L2101;
    }
  goto ret0;

 L2101: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L2102;
  goto ret0;

 L2102: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2103;
    }
  goto ret0;

 L2103: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT&& reload_completed))
    {
      return gen_split_335 (operands);
    }
  goto ret0;

 L5577: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2147;
    }
  goto ret0;

 L2147: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L2148;
  goto ret0;

 L2148: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L2149;
    }
  goto ret0;

 L2149: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L2150;
    }
  goto ret0;

 L2150: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2151;
  goto ret0;

 L2151: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2152;
    }
  goto ret0;

 L2152: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2153;
  goto ret0;

 L2153: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2154;
    }
  goto ret0;

 L2154: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2155;
  goto ret0;

 L2155: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L2156;
    }
  goto ret0;

 L2156: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L2157;
  goto ret0;

 L2157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2158;
    }
  goto ret0;

 L2158: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed))
    {
      return gen_split_341 (operands);
    }
  goto ret0;

 L2105: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L2106;
    case CALL:
      goto L2307;
    default:
     break;
   }
  goto ret0;

 L2106: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5580;
    case SFmode:
      goto L5581;
    case DFmode:
      goto L5582;
    default:
      break;
    }
 L2331: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L2332;

 L5580: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2107;
    }
  goto L2331;

 L2107: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5583;
  x2 = XEXP (x1, 0);
  goto L2331;

 L5583: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT:
      goto L2108;
    case DIV:
      goto L2193;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2108: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L2109;
  x2 = XEXP (x1, 0);
  goto L2331;

 L2109: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2110;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2110: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L2111;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2111: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2112;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2112: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2113;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2113: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2114;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2114: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2115;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2115: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2116;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2116: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2117;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2117: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR&& reload_completed))
    {
      return gen_split_336 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2193: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2194;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2194: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L2195;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2195: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2196;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2196: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2197;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2197: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2198;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2198: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2199;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2199: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2200;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2200: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2201;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2201: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed))
    {
      return gen_split_345 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L5581: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L2121;
    }
  goto L2331;

 L2121: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode
      && GET_CODE (x2) == DIV)
    goto L2122;
  x2 = XEXP (x1, 0);
  goto L2331;

 L2122: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L2123;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2123: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L2124;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2124: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2125;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2125: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2126;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2126: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2127;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2127: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2128;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2128: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2129;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2129: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2130;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2130: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed))
    {
      return gen_split_338 (operands);
    }
 L2143: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed))
    {
      return gen_split_339 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L5582: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2162;
    }
  goto L2331;

 L2162: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L2163;
  x2 = XEXP (x1, 0);
  goto L2331;

 L2163: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L2164;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2164: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L2165;
    }
  x2 = XEXP (x1, 0);
  goto L2331;

 L2165: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2166;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2166: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2167;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2167: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2168;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2168: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      operands[4] = x2;
      goto L2169;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2169: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2170;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2170: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2171;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2171: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed))
    {
      return gen_split_342 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2331;

 L2332: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L2333;
  goto ret0;

 L2333: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L2334;
  goto ret0;

 L2334: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L2335;
    }
  goto ret0;

 L2335: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1L)
    goto L2336;
  goto ret0;

 L2336: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2337;
  goto ret0;

 L2337: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2338;
    }
  goto ret0;

 L2338: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2339;
  goto ret0;

 L2339: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2340;
    }
  goto ret0;

 L2340: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2341;
  goto ret0;

 L2341: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L2342;
    }
  goto ret0;

 L2342: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && find_reg_note (insn, REG_NORETURN, NULL_RTX)))
    {
      return gen_split_403 (operands);
    }
 L2356: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_404 (operands);
    }
  goto ret0;

 L2307: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == MEM)
    goto L2308;
  goto ret0;

 L2308: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L2309;
    }
  goto ret0;

 L2309: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1L)
    goto L2310;
  goto ret0;

 L2310: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2311;
  goto ret0;

 L2311: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2312;
    }
  goto ret0;

 L2312: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2313;
  goto ret0;

 L2313: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2314;
    }
  goto ret0;

 L2314: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2315;
  goto ret0;

 L2315: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2316;
    }
  goto ret0;

 L2316: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && find_reg_note (insn, REG_NORETURN, NULL_RTX)))
    {
      return gen_split_401 (operands);
    }
 L2328: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_402 (operands);
    }
  goto ret0;

 L2173: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L2174;
  goto ret0;

 L2174: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2175;
    }
  goto ret0;

 L2175: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == DIV)
    goto L2176;
  goto ret0;

 L2176: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2177;
    }
  goto ret0;

 L2177: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L2178;
    }
  goto ret0;

 L2178: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2179;
  goto ret0;

 L2179: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2180;
    }
  goto ret0;

 L2180: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2181;
  goto ret0;

 L2181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2182;
    }
  goto ret0;

 L2182: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2183;
  goto ret0;

 L2183: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L2184;
    }
  goto ret0;

 L2184: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L2185;
  goto ret0;

 L2185: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[6] = x2;
      goto L2186;
    }
  goto ret0;

 L2186: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == CLOBBER)
    goto L2187;
  goto ret0;

 L2187: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[7] = x2;
      goto L2188;
    }
  goto ret0;

 L2188: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed))
    {
      return gen_split_344 (operands);
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
      goto L1909;
    case PARALLEL:
      goto L5535;
    case UNSPEC_VOLATILE:
      goto L5540;
    default:
     break;
   }
  goto ret0;

 L1909: ATTRIBUTE_UNUSED_LABEL
  return split_1 (x0, insn);

 L5535: ATTRIBUTE_UNUSED_LABEL
  return split_2 (x0, insn);

 L5540: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1
      && XINT (x0, 1) == 7)
    goto L2368;
  goto ret0;

 L2368: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  operands[0] = x1;
  goto L2369;

 L2369: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_416 (operands);
    }
  goto ret0;
 ret0:
  return 0;
}

rtx peephole2_insns PARAMS ((rtx, rtx, int *));
rtx
peephole2_insns (x0, insn, _pmatch_len)
     rtx x0 ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
     int *_pmatch_len ATTRIBUTE_UNUSED;
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

  if (GET_CODE (x0) == SET)
    goto L2047;
  goto ret0;

 L2047: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L2048;
    }
  goto ret0;

 L2048: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L2049;
    }
  goto ret0;

 L2049: ATTRIBUTE_UNUSED_LABEL
  tem = peep2_next_insn (1);
  if (tem == NULL_RTX)
    goto ret0;
  x1 = PATTERN (tem);
  if (GET_CODE (x1) == SET)
    goto L2050;
  goto ret0;

 L2050: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, CCImode))
    {
      operands[2] = x2;
      goto L2051;
    }
  goto ret0;

 L2051: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, CCImode))
    {
      operands[3] = x2;
      goto L2052;
    }
  goto ret0;

 L2052: ATTRIBUTE_UNUSED_LABEL
  tem = peep2_next_insn (2);
  if (tem == NULL_RTX)
    goto ret0;
  x1 = PATTERN (tem);
  if (GET_CODE (x1) == SET)
    goto L2053;
  goto ret0;

 L2053: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, CCImode))
    {
      operands[4] = x2;
      goto L2054;
    }
  goto ret0;

 L2054: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, CCImode))
    {
      operands[5] = x2;
      goto L2055;
    }
  goto ret0;

 L2055: ATTRIBUTE_UNUSED_LABEL
  tem = peep2_next_insn (3);
  if (tem == NULL_RTX)
    goto ret0;
  x1 = PATTERN (tem);
  if (GET_CODE (x1) == SET)
    goto L2056;
  goto ret0;

 L2056: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2057;
    }
  goto ret0;

 L2057: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 15)
    goto L2058;
  goto ret0;

 L2058: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[6])
      && (REGNO (operands[3]) == REGNO (operands[0])
   && REGNO (operands[4]) == REGNO (operands[0]) + 1
   && REGNO (operands[4]) == REGNO (operands[2]) + 1
   && REGNO (operands[6]) == REGNO (operands[2])))
    {
      *_pmatch_len = 3;
      tem = gen_peephole2_313 (insn, operands);
      if (tem != 0)
        return tem;
    }
  goto ret0;
 ret0:
  return 0;
}

