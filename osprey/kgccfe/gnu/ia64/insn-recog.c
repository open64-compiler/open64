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


extern rtx gen_split_274 PARAMS ((rtx *));
extern rtx gen_split_275 PARAMS ((rtx *));
extern rtx gen_split_279 PARAMS ((rtx *));
extern rtx gen_split_281 PARAMS ((rtx *));
extern rtx gen_split_282 PARAMS ((rtx *));
extern rtx gen_split_291 PARAMS ((rtx *));
extern rtx gen_split_292 PARAMS ((rtx *));
extern rtx gen_split_299 PARAMS ((rtx *));
extern rtx gen_split_300 PARAMS ((rtx *));
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
extern rtx gen_peephole2_311 PARAMS ((rtx, rtx *));
extern rtx gen_split_321 PARAMS ((rtx *));
extern rtx gen_split_322 PARAMS ((rtx *));
extern rtx gen_split_333 PARAMS ((rtx *));
extern rtx gen_split_334 PARAMS ((rtx *));
extern rtx gen_split_336 PARAMS ((rtx *));
extern rtx gen_split_337 PARAMS ((rtx *));
extern rtx gen_split_339 PARAMS ((rtx *));
extern rtx gen_split_340 PARAMS ((rtx *));
extern rtx gen_split_342 PARAMS ((rtx *));
extern rtx gen_split_343 PARAMS ((rtx *));
extern rtx gen_split_348 PARAMS ((rtx *));
extern rtx gen_split_350 PARAMS ((rtx *));
extern rtx gen_split_351 PARAMS ((rtx *));
extern rtx gen_split_385 PARAMS ((rtx *));
extern rtx gen_split_386 PARAMS ((rtx *));
extern rtx gen_split_387 PARAMS ((rtx *));
extern rtx gen_split_388 PARAMS ((rtx *));
extern rtx gen_split_389 PARAMS ((rtx *));
extern rtx gen_split_390 PARAMS ((rtx *));
extern rtx gen_split_391 PARAMS ((rtx *));
extern rtx gen_split_392 PARAMS ((rtx *));
extern rtx gen_split_393 PARAMS ((rtx *));
extern rtx gen_split_399 PARAMS ((rtx *));
extern rtx gen_split_400 PARAMS ((rtx *));
extern rtx gen_split_401 PARAMS ((rtx *));
extern rtx gen_split_402 PARAMS ((rtx *));
extern rtx gen_split_403 PARAMS ((rtx *));
extern rtx gen_split_414 PARAMS ((rtx *));



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
      goto L318;
    case IOR:
      goto L329;
    case NOT:
      goto L347;
    case EQ:
      goto L1453;
    case NE:
      goto L1461;
    default:
     break;
   }
 L4772: ATTRIBUTE_UNUSED_LABEL
  if (normal_comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L1418;
    }
  if (adjusted_comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L1423;
    }
 L4774: ATTRIBUTE_UNUSED_LABEL
  if (comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L1438;
    }
  goto ret0;

 L318: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L4778;
  goto ret0;

 L4778: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case NOT:
      goto L324;
    case NE:
      goto L413;
    case EQ:
      goto L422;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L4777;
    default:
      goto L4779;
   }
 L4777: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L319;
    }
 L4779: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x2, BImode))
    {
      operands[4] = x2;
      goto L383;
    }
  if (signed_inequality_operator (x2, BImode))
    {
      operands[3] = x2;
      goto L360;
    }
  goto ret0;

 L324: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L4783;
  goto ret0;

 L4783: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L325;
    }
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L398;
    }
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L376;
    }
  goto ret0;

 L325: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 62;
    }
  goto ret0;

 L398: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L399;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L369;
    }
  goto ret0;

 L399: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L400;
    }
  goto ret0;

 L400: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 72;
    }
  goto ret0;

 L369: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L370;
    }
  goto ret0;

 L370: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 68;
    }
  goto ret0;

 L376: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L4786;
    case DImode:
      goto L4787;
    default:
      break;
    }
  goto ret0;

 L4786: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L377;
    }
  goto ret0;

 L377: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L378;
  goto ret0;

 L378: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 69;
    }
  goto ret0;

 L4787: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L407;
    }
  goto ret0;

 L407: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L408;
  goto ret0;

 L408: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 73;
    }
  goto ret0;

 L413: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4788;
  goto L4779;

 L4788: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L414;
    case ZERO_EXTRACT:
      goto L432;
    default:
     break;
   }
  goto L4779;

 L414: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L415;
    }
  goto L4779;

 L415: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L416;
  goto L4779;

 L416: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L417;
  goto L4779;

 L417: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 74;
    }
  x2 = XEXP (x1, 0);
  goto L4779;

 L432: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L433;
    }
  goto L4779;

 L433: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L434;
  goto L4779;

 L434: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L435;
    }
  goto L4779;

 L435: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L436;
  goto L4779;

 L436: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 76;
    }
  x2 = XEXP (x1, 0);
  goto L4779;

 L422: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4790;
  goto L4779;

 L4790: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L423;
    case ZERO_EXTRACT:
      goto L442;
    default:
     break;
   }
  goto L4779;

 L423: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L424;
    }
  goto L4779;

 L424: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L425;
  goto L4779;

 L425: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L426;
  goto L4779;

 L426: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 75;
    }
  x2 = XEXP (x1, 0);
  goto L4779;

 L442: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L443;
    }
  goto L4779;

 L443: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L444;
  goto L4779;

 L444: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L445;
    }
  goto L4779;

 L445: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L446;
  goto L4779;

 L446: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 77;
    }
  x2 = XEXP (x1, 0);
  goto L4779;

 L319: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 61;
    }
  goto ret0;

 L383: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L384;
    }
  if (gr_reg_or_0_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L354;
    }
  goto ret0;

 L384: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L385;
    }
  goto ret0;

 L385: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 70;
    }
  goto ret0;

 L354: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L355;
    }
  goto ret0;

 L355: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 66;
    }
  goto ret0;

 L360: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L4792;
    case DImode:
      goto L4793;
    default:
      break;
    }
  goto ret0;

 L4792: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L361;
    }
  goto ret0;

 L361: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L362;
  goto ret0;

 L362: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 67;
    }
  goto ret0;

 L4793: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L391;
    }
  goto ret0;

 L391: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L392;
  goto ret0;

 L392: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 71;
    }
  goto ret0;

 L329: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L4795;
  goto ret0;

 L4795: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case NOT:
      goto L335;
    case NE:
      goto L511;
    case EQ:
      goto L520;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L4794;
    default:
      goto L4796;
   }
 L4794: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L330;
    }
 L4796: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x2, BImode))
    {
      operands[4] = x2;
      goto L481;
    }
  if (signed_inequality_operator (x2, BImode))
    {
      operands[3] = x2;
      goto L458;
    }
  goto ret0;

 L335: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L4800;
  goto ret0;

 L4800: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L336;
    }
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L496;
    }
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L474;
    }
  goto ret0;

 L336: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 64;
    }
  goto ret0;

 L496: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L497;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L467;
    }
  goto ret0;

 L497: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L498;
    }
  goto ret0;

 L498: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 84;
    }
  goto ret0;

 L467: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L468;
    }
  goto ret0;

 L468: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 80;
    }
  goto ret0;

 L474: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L4803;
    case DImode:
      goto L4804;
    default:
      break;
    }
  goto ret0;

 L4803: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L475;
    }
  goto ret0;

 L475: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L476;
  goto ret0;

 L476: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 81;
    }
  goto ret0;

 L4804: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L505;
    }
  goto ret0;

 L505: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L506;
  goto ret0;

 L506: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 85;
    }
  goto ret0;

 L511: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4805;
  goto L4796;

 L4805: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L512;
    case ZERO_EXTRACT:
      goto L530;
    default:
     break;
   }
  goto L4796;

 L512: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L513;
    }
  goto L4796;

 L513: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L514;
  goto L4796;

 L514: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L515;
  goto L4796;

 L515: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 86;
    }
  x2 = XEXP (x1, 0);
  goto L4796;

 L530: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L531;
    }
  goto L4796;

 L531: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L532;
  goto L4796;

 L532: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L533;
    }
  goto L4796;

 L533: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L534;
  goto L4796;

 L534: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 88;
    }
  x2 = XEXP (x1, 0);
  goto L4796;

 L520: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4807;
  goto L4796;

 L4807: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case AND:
      goto L521;
    case ZERO_EXTRACT:
      goto L540;
    default:
     break;
   }
  goto L4796;

 L521: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L522;
    }
  goto L4796;

 L522: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L523;
  goto L4796;

 L523: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L524;
  goto L4796;

 L524: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 87;
    }
  x2 = XEXP (x1, 0);
  goto L4796;

 L540: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L541;
    }
  goto L4796;

 L541: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L542;
  goto L4796;

 L542: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L543;
    }
  goto L4796;

 L543: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L544;
  goto L4796;

 L544: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      return 89;
    }
  x2 = XEXP (x1, 0);
  goto L4796;

 L330: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      return 63;
    }
  goto ret0;

 L481: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L482;
    }
  if (gr_reg_or_0_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L452;
    }
  goto ret0;

 L482: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L483;
    }
  goto ret0;

 L483: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 82;
    }
  goto ret0;

 L452: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L453;
    }
  goto ret0;

 L453: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 78;
    }
  goto ret0;

 L458: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L4809;
    case DImode:
      goto L4810;
    default:
      break;
    }
  goto ret0;

 L4809: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L459;
    }
  goto ret0;

 L459: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L460;
  goto ret0;

 L460: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 79;
    }
  goto ret0;

 L4810: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L489;
    }
  goto ret0;

 L489: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L490;
  goto ret0;

 L490: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      return 83;
    }
  goto ret0;

 L347: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L348;
    }
  goto ret0;

 L348: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 65;
    }
  goto ret0;

 L1453: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ZERO_EXTRACT)
    goto L1454;
  goto L4772;

 L1454: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1455;
    }
  goto L4772;

 L1455: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    goto L1456;
  goto L4772;

 L1456: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (immediate_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1457;
    }
  goto L4772;

 L1457: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return 210;
    }
  goto L4772;

 L1461: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ZERO_EXTRACT)
    goto L1462;
  goto L4772;

 L1462: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1463;
    }
  goto L4772;

 L1463: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    goto L1464;
  goto L4772;

 L1464: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (immediate_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1465;
    }
  goto L4772;

 L1465: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return 211;
    }
  goto L4772;

 L1418: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1419;
    }
  if (gr_reg_or_0_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1429;
    }
  goto L4774;

 L1419: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_operand (x2, SImode))
    {
      operands[3] = x2;
      return 203;
    }
  goto L4774;

 L1429: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_operand (x2, DImode))
    {
      operands[3] = x2;
      return 205;
    }
  goto L4774;

 L1423: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L4811;
    case DImode:
      goto L4812;
    default:
      break;
    }
  goto L4774;

 L4811: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1424;
    }
  goto L4774;

 L1424: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_adjusted_operand (x2, SImode))
    {
      operands[3] = x2;
      return 204;
    }
  goto L4774;

 L4812: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1434;
    }
  goto L4774;

 L1434: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_8bit_adjusted_operand (x2, DImode))
    {
      operands[3] = x2;
      return 206;
    }
  goto L4774;

 L1438: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L4813;
    case DFmode:
      goto L4814;
    case TFmode:
      goto L4815;
    default:
      break;
    }
  goto ret0;

 L4813: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L1439;
    }
  goto ret0;

 L1439: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 207;
    }
  goto ret0;

 L4814: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L1444;
    }
  goto ret0;

 L1444: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 208;
    }
  goto ret0;

 L4815: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1449;
    }
  goto ret0;

 L1449: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      return 209;
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
    goto L270;
  if (destination_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1516;
    }
 L4748: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L40;
    }
 L4754: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L156;
    }
 L4755: ATTRIBUTE_UNUSED_LABEL
  if (grfr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L164;
    }
 L4759: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L208;
    }
  goto ret0;

 L270: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L271;
    }
  goto ret0;

 L271: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT)
    goto L4835;
  goto ret0;

 L4835: ATTRIBUTE_UNUSED_LABEL
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L272;
    }
 L4836: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x2, 0) == 32LL)
    goto L296;
  goto ret0;

 L272: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L273;
    }
  x2 = XEXP (x1, 1);
  goto L4836;

 L273: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, DImode))
    {
      operands[3] = x1;
      goto L274;
    }
  x1 = XEXP (x0, 0);
  x2 = XEXP (x1, 1);
  goto L4836;

 L274: ATTRIBUTE_UNUSED_LABEL
  if (((gr_register_operand (operands[3], DImode) && INTVAL (operands[1]) <= 16)
   || operands[3] == const0_rtx || operands[3] == constm1_rtx))
    {
      return 55;
    }
  x1 = XEXP (x0, 0);
  x2 = XEXP (x1, 1);
  goto L4836;

 L296: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == CONST_INT)
    goto L4837;
  goto ret0;

 L4837: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x2, 0) == XWINT (x2, 0))
    switch ((int) XWINT (x2, 0))
      {
      case 0LL:
        goto L297;
      case 32LL:
        goto L305;
      default:
        break;
      }
  goto ret0;

 L297: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == LSHIFTRT)
    goto L298;
  goto ret0;

 L298: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L299;
    }
  goto ret0;

 L299: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 32LL)
    {
      return 58;
    }
  goto ret0;

 L305: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (gr_reg_or_0_operand (x1, DImode))
    {
      operands[1] = x1;
      return 59;
    }
  goto ret0;

 L1516: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == IF_THEN_ELSE)
    goto L1517;
  if (move_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L37;
    }
  x1 = XEXP (x0, 0);
  goto L4748;

 L1517: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1518;
    }
  x1 = XEXP (x0, 0);
  goto L4748;

 L1518: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1519;
    }
  x1 = XEXP (x0, 0);
  goto L4748;

 L1519: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1520;
  x1 = XEXP (x0, 0);
  goto L4748;

 L1520: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1521;
    }
  x1 = XEXP (x0, 0);
  goto L4748;

 L1521: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (move_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1522;
    }
  x1 = XEXP (x0, 0);
  goto L4748;

 L1522: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[2])
   && ia64_move_ok (operands[0], operands[3])))
    {
      return 217;
    }
  x1 = XEXP (x0, 0);
  goto L4748;

 L37: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 7;
    }
  x1 = XEXP (x0, 0);
  goto L4748;

 L40: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L4839;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4839: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L41;
    case MINUS:
      goto L51;
    case LO_SUM:
      goto L62;
    case UNSPEC:
      goto L4845;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4754;

 L41: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L4848;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4848: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case REG:
      goto L4852;
    case HIGH:
      goto L57;
    case PLUS:
      goto L689;
    default:
     break;
   }
 L4850: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L89;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L4852: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 0) == 1)
    goto L68;
  goto L4850;

 L68: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L4853;
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
  goto L4850;

 L4853: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L4856;
  goto L42;

 L4856: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1)
    goto L4859;
  goto L42;

 L4859: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x2, 1))
    {
    case 0LL:
      goto L69;
    case 1LL:
      goto L75;
    case 3LL:
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
  goto L4754;

 L58: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 11;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L689: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L690;
  x1 = XEXP (x0, 0);
  goto L4754;

 L690: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L691;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L691: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L692;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L692: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L693;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L693: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L694;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L694: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 108;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L89: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L4862;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4862: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L4864;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4864: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1)
    goto L4866;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4866: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x2, 1))
    {
    case 2LL:
      goto L90;
    case 4LL:
      goto L119;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L90: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L91;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

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
  goto L4754;

 L119: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L120;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

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
  goto L4754;

 L51: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L52;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L52: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1)
    {
      return 10;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L62: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L63;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L63: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (got_symbolic_operand (x2, VOIDmode))
    {
      operands[2] = x2;
      return 12;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L4845: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1)
    goto L4868;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4868: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x1, 1))
    {
    case 2LL:
      goto L79;
    case 4LL:
      goto L108;
    case 21LL:
      goto L1738;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L79: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_MODE (x2) == DImode)
    goto L4871;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4871: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L80;
    }
 L4872: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      return 16;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L80: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 15;
    }
  x1 = XEXP (x0, 1);
  x2 = XVECEXP (x1, 0, 0);
  goto L4872;

 L108: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_MODE (x2) == DImode)
    goto L4873;
  x1 = XEXP (x0, 0);
  goto L4754;

 L4873: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L109;
    }
 L4874: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      return 21;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L109: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 20;
    }
  x1 = XEXP (x0, 1);
  x2 = XVECEXP (x1, 0, 0);
  goto L4874;

 L1738: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return 244;
    }
  x1 = XEXP (x0, 0);
  goto L4754;

 L156: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L4875;
  x1 = XEXP (x0, 0);
  goto L4755;

 L4875: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case SIGN_EXTEND:
      goto L157;
    case ZERO_EXTEND:
      goto L169;
    case SIGN_EXTRACT:
      goto L259;
    case ZERO_EXTRACT:
      goto L265;
    case AND:
      goto L278;
    case IOR:
      goto L309;
    case PLUS:
      goto L622;
    case MINUS:
      goto L641;
    case NEG:
      goto L718;
    case UNSPEC:
      goto L4894;
    case ASHIFT:
      goto L1347;
    case ASHIFTRT:
      goto L1369;
    case LSHIFTRT:
      goto L1374;
    case ROTATERT:
      goto L1379;
    case ROTATE:
      goto L1384;
    case NOT:
      goto L1414;
    case NE:
      goto L1507;
    case EQ:
      goto L1512;
    case IF_THEN_ELSE:
      goto L1526;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4755;

 L157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case QImode:
      goto L4895;
    case HImode:
      goto L4896;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L4895: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, QImode))
    {
      operands[1] = x2;
      return 29;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L4896: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, HImode))
    {
      operands[1] = x2;
      return 30;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L169: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case QImode:
      goto L4897;
    case HImode:
      goto L4898;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L4897: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x2, QImode))
    {
      operands[1] = x2;
      return 32;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L4898: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x2, HImode))
    {
      operands[1] = x2;
      return 33;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L259: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L260;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L260: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L261;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L261: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      return 53;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L265: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L266;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L266: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L267;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L267: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      return 54;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L278: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ASHIFT)
    goto L279;
  x1 = XEXP (x0, 0);
  goto L4755;

 L279: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L280;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L280: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L281;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L281: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L282;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L282: ATTRIBUTE_UNUSED_LABEL
  if ((CONST_OK_FOR_M (INTVAL (operands[2]))
   && ia64_depz_field_mask (operands[3], operands[2]) > 0))
    {
      return 56;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L309: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ZERO_EXTEND)
    goto L310;
  x1 = XEXP (x0, 0);
  goto L4755;

 L310: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L311;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L311: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == ASHIFT)
    goto L312;
  x1 = XEXP (x0, 0);
  goto L4755;

 L312: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTEND)
    goto L313;
  x1 = XEXP (x0, 0);
  goto L4755;

 L313: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L314;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L314: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL)
    {
      return 60;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L622: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L4900;
  x1 = XEXP (x0, 0);
  goto L4755;

 L4900: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L628;
    case MULT:
      goto L635;
    case NOT:
      goto L647;
    case SUBREG:
    case REG:
      goto L4899;
    default:
      x1 = XEXP (x0, 0);
      goto L4755;
   }
 L4899: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L623;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L628: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L4904;
  x1 = XEXP (x0, 0);
  goto L4755;

 L4904: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L1361;
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L629;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1361: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L1362;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1362: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L1363;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1363: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L1364;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1364: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L1365;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1365: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 192;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L629: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L630;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L630: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    {
      return 102;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L635: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L636;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L636: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT)
    goto L4905;
  x1 = XEXP (x0, 0);
  goto L4755;

 L4905: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x3, 0) == 2LL)
    goto L637;
 L4906: ATTRIBUTE_UNUSED_LABEL
  if (shladd_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1355;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L637: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    {
      return 103;
    }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 1);
  goto L4906;

 L1355: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[3] = x2;
      return 191;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L647: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L648;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L648: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 105;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L623: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_22bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 101;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L641: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_reg_or_8bit_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L642;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L642: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 104;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L718: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 111;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L4894: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 16)
    goto L722;
  x1 = XEXP (x0, 0);
  goto L4755;

 L722: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 112;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1347: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1348;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1348: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_6bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 190;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1369: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1370;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1370: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_6bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 193;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1374: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1375;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1375: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_6bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 194;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1379: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1380;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1380: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_count_operand (x2, DImode))
    {
      operands[2] = x2;
      return 195;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1384: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1385;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1385: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_count_operand (x2, DImode))
    {
      operands[2] = x2;
      return 196;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1414: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 202;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1507: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1508;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1508: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return 215;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1512: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1513;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1513: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return 216;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1526: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1527;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1527: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1528;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1528: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1529;
  x1 = XEXP (x0, 0);
  goto L4755;

 L1529: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == NEG)
    goto L1530;
  x1 = XEXP (x0, 0);
  goto L4755;

 L1530: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1531;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L1531: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, DImode))
    {
      operands[3] = x2;
      return 218;
    }
  x1 = XEXP (x0, 0);
  goto L4755;

 L164: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L4907;
  x1 = XEXP (x0, 0);
  goto L4759;

 L4907: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case SIGN_EXTEND:
      goto L165;
    case ZERO_EXTEND:
      goto L177;
    case AND:
      goto L1393;
    case IOR:
      goto L1404;
    case XOR:
      goto L1409;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4759;

 L165: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 31;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L177: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_nonimmediate_operand (x2, SImode))
    {
      operands[1] = x2;
      return 34;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1393: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L4913;
  x1 = XEXP (x0, 0);
  goto L4759;

 L4913: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L1399;
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1394;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1399: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1400;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1400: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 199;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1394: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 198;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1404: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1405;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1405: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 200;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1409: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1410;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L1410: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_reg_or_8bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 201;
    }
  x1 = XEXP (x0, 0);
  goto L4759;

 L208: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L4914;
  goto ret0;

 L4914: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case FIX:
      goto L209;
    case UNSIGNED_FIX:
      goto L240;
    case MULT:
      goto L652;
    case PLUS:
      goto L667;
    case TRUNCATE:
      goto L698;
    default:
     break;
   }
  goto ret0;

 L209: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L4919;
    case DFmode:
      goto L4920;
    case TFmode:
      goto L4921;
    default:
      break;
    }
  goto ret0;

 L4919: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 42;
    }
  goto ret0;

 L4920: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 43;
    }
  goto ret0;

 L4921: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 44;
    }
  goto ret0;

 L240: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L4922;
    case DFmode:
      goto L4923;
    case TFmode:
      goto L4924;
    default:
      break;
    }
  goto ret0;

 L4922: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 49;
    }
  goto ret0;

 L4923: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 50;
    }
  goto ret0;

 L4924: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 51;
    }
  goto ret0;

 L652: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L653;
    }
  goto ret0;

 L653: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 106;
    }
  goto ret0;

 L667: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MULT)
    goto L668;
  goto ret0;

 L668: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L669;
    }
  goto ret0;

 L669: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L670;
    }
  goto ret0;

 L670: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L671;
    }
  goto ret0;

 L671: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 107;
    }
  goto ret0;

 L698: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TImode
      && GET_CODE (x2) == LSHIFTRT)
    goto L699;
  goto ret0;

 L699: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode
      && GET_CODE (x3) == MULT)
    goto L700;
  goto ret0;

 L700: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TImode)
    goto L4925;
  goto ret0;

 L4925: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case SIGN_EXTEND:
      goto L701;
    case ZERO_EXTEND:
      goto L711;
    default:
     break;
   }
  goto ret0;

 L701: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L702;
    }
  goto ret0;

 L702: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == SIGN_EXTEND)
    goto L703;
  goto ret0;

 L703: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L704;
    }
  goto ret0;

 L704: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 64LL)
    {
      return 109;
    }
  goto ret0;

 L711: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L712;
    }
  goto ret0;

 L712: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == ZERO_EXTEND)
    goto L713;
  goto ret0;

 L713: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L714;
    }
  goto ret0;

 L714: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 64LL)
    {
      return 110;
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
      goto L193;
    case UNSIGNED_FLOAT:
      goto L228;
    case PLUS:
      goto L770;
    case MINUS:
      goto L775;
    case MULT:
      goto L780;
    case ABS:
      goto L785;
    case NEG:
      goto L789;
    case SMIN:
      goto L798;
    case SMAX:
      goto L803;
    case DIV:
      goto L849;
    default:
     break;
   }
  goto ret0;

 L193: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DFmode:
      goto L4939;
    case TFmode:
      goto L4943;
    default:
      break;
    }
  goto ret0;

 L4939: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L880;
    case MINUS:
      goto L891;
    case MULT:
      goto L902;
    case NEG:
      goto L967;
    case SUBREG:
    case REG:
      goto L4937;
    default:
      goto ret0;
   }
 L4937: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 38;
    }
  goto ret0;

 L880: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L4948;
  goto ret0;

 L4948: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L939;
    case NEG:
      goto L994;
    case SUBREG:
    case REG:
      goto L4947;
    default:
      goto ret0;
   }
 L4947: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L881;
    }
  goto ret0;

 L939: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L940;
    }
  goto ret0;

 L940: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L941;
    }
  goto ret0;

 L941: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 141;
    }
  goto ret0;

 L994: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L995;
  goto ret0;

 L995: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L996;
    }
  goto ret0;

 L996: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L997;
    }
  goto ret0;

 L997: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 148;
    }
  goto ret0;

 L881: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 130;
    }
  goto ret0;

 L891: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L4951;
  goto ret0;

 L4951: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L954;
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L892;
    }
  goto ret0;

 L954: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L955;
    }
  goto ret0;

 L955: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L956;
    }
  goto ret0;

 L956: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 143;
    }
  goto ret0;

 L892: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 132;
    }
  goto ret0;

 L902: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L903;
    }
  goto ret0;

 L903: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 134;
    }
  goto ret0;

 L967: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L968;
  goto ret0;

 L968: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L969;
    }
  goto ret0;

 L969: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 145;
    }
  goto ret0;

 L4943: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L1047;
    case MINUS:
      goto L1064;
    case MULT:
      goto L1081;
    case NEG:
      goto L1215;
    case SUBREG:
    case REG:
      goto L4938;
    default:
      goto ret0;
   }
 L4938: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 39;
    }
  goto ret0;

 L1047: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L4953;
  goto ret0;

 L4953: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1150;
    case NEG:
      goto L1238;
    case REG:
    case CONST_DOUBLE:
      goto L4952;
    default:
      goto ret0;
   }
 L4952: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1048;
    }
  goto ret0;

 L1150: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1151;
    }
  goto ret0;

 L1151: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1152;
    }
  goto ret0;

 L1152: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 169;
    }
  goto ret0;

 L1238: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L1239;
  goto ret0;

 L1239: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1240;
    }
  goto ret0;

 L1240: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1241;
    }
  goto ret0;

 L1241: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 180;
    }
  goto ret0;

 L1048: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 152;
    }
  goto ret0;

 L1064: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L4956;
  goto ret0;

 L4956: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L1194;
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1065;
    }
  goto ret0;

 L1194: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1195;
    }
  goto ret0;

 L1195: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1196;
    }
  goto ret0;

 L1196: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 174;
    }
  goto ret0;

 L1065: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 155;
    }
  goto ret0;

 L1081: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1082;
    }
  goto ret0;

 L1082: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 158;
    }
  goto ret0;

 L1215: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1216;
  goto ret0;

 L1216: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1217;
    }
  goto ret0;

 L1217: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 177;
    }
  goto ret0;

 L228: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 46;
    }
  goto ret0;

 L770: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L4958;
  goto ret0;

 L4958: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L809;
    case NEG:
      goto L829;
    case SUBREG:
    case REG:
      goto L4957;
    default:
      goto ret0;
   }
 L4957: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L771;
    }
  goto ret0;

 L809: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L810;
    }
  goto ret0;

 L810: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L811;
    }
  goto ret0;

 L811: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 123;
    }
  goto ret0;

 L829: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == MULT)
    goto L830;
  goto ret0;

 L830: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L831;
    }
  goto ret0;

 L831: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      goto L832;
    }
  goto ret0;

 L832: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 126;
    }
  goto ret0;

 L771: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 115;
    }
  goto ret0;

 L775: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L4961;
  goto ret0;

 L4961: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L816;
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L776;
    }
  goto ret0;

 L816: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L817;
    }
  goto ret0;

 L817: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L818;
    }
  goto ret0;

 L818: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[3] = x2;
      return 124;
    }
  goto ret0;

 L776: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 116;
    }
  goto ret0;

 L780: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L781;
    }
  goto ret0;

 L781: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 117;
    }
  goto ret0;

 L785: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 118;
    }
  goto ret0;

 L789: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SFmode)
    goto L4963;
  goto ret0;

 L4963: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L794;
    case MULT:
      goto L823;
    case SUBREG:
    case REG:
      goto L4962;
    default:
      goto ret0;
   }
 L4962: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 119;
    }
  goto ret0;

 L794: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 120;
    }
  goto ret0;

 L823: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L824;
    }
  goto ret0;

 L824: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 125;
    }
  goto ret0;

 L798: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L799;
    }
  goto ret0;

 L799: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 121;
    }
  goto ret0;

 L803: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L804;
    }
  goto ret0;

 L804: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, SFmode))
    {
      operands[2] = x2;
      return 122;
    }
  goto ret0;

 L849: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L850;
    }
  goto ret0;

 L850: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, SFmode))
    {
      operands[2] = x2;
      goto L851;
    }
  goto ret0;

 L851: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 127;
    }
 L870: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 128;
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
      goto L4742;
    case BImode:
      goto L4743;
    case QImode:
      goto L4744;
    case HImode:
      goto L4745;
    case SImode:
      goto L4746;
    case DImode:
      goto L4760;
    case TImode:
      goto L4749;
    case SFmode:
      goto L4751;
    case DFmode:
      goto L4752;
    case TFmode:
      goto L4753;
    default:
      break;
    }
 L1467: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == PC)
    goto L1672;
 L1772: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x1;
  goto L1773;
 L1828: ATTRIBUTE_UNUSED_LABEL
  switch (GET_MODE (x1))
    {
    case BImode:
      goto L4767;
    case DImode:
      goto L4768;
    default:
      break;
    }
  goto ret0;

 L4742: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, CCImode))
    {
      operands[0] = x1;
      goto L2;
    }
  goto L1467;

 L2: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, CCImode))
    {
      operands[1] = x1;
      return 0;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L4743: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L5;
    }
 L4761: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L317;
    }
  goto L1467;

 L5: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, BImode))
    {
      operands[1] = x1;
      return 1;
    }
  x1 = XEXP (x0, 0);
  goto L4761;

 L317: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BImode)
    goto L4769;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4769: ATTRIBUTE_UNUSED_LABEL
  tem = recog_1 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4744: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, QImode))
    {
      operands[0] = x1;
      goto L8;
    }
  goto L1467;

 L8: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, QImode))
    {
      operands[1] = x1;
      goto L9;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L9: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 2;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L4745: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L12;
    }
 L4762: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, HImode))
    {
      operands[0] = x1;
      goto L547;
    }
  goto L1467;

 L12: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, HImode))
    {
      operands[1] = x1;
      goto L13;
    }
  x1 = XEXP (x0, 0);
  goto L4762;

 L13: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 3;
    }
  x1 = XEXP (x0, 0);
  goto L4762;

 L547: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == HImode
      && GET_CODE (x1) == MULT)
    goto L548;
  x1 = XEXP (x0, 0);
  goto L1467;

 L548: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L549;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L549: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, HImode))
    {
      operands[2] = x2;
      return 90;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L4746: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L1534;
    }
 L4763: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L552;
    }
 L4764: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L589;
    }
  goto L1467;

 L1534: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == IF_THEN_ELSE)
    goto L1535;
  if (move_operand (x1, SImode))
    {
      operands[1] = x1;
      goto L25;
    }
  x1 = XEXP (x0, 0);
  goto L4763;

 L1535: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1536;
    }
  x1 = XEXP (x0, 0);
  goto L4763;

 L1536: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1537;
    }
  x1 = XEXP (x0, 0);
  goto L4763;

 L1537: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1538;
  x1 = XEXP (x0, 0);
  goto L4763;

 L1538: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L1539;
    }
  x1 = XEXP (x0, 0);
  goto L4763;

 L1539: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (move_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L1540;
    }
  x1 = XEXP (x0, 0);
  goto L4763;

 L1540: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[2])
   && ia64_move_ok (operands[0], operands[3])))
    {
      return 219;
    }
  x1 = XEXP (x0, 0);
  goto L4763;

 L25: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 5;
    }
  x1 = XEXP (x0, 0);
  goto L4763;

 L552: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L4816;
  x1 = XEXP (x0, 0);
  goto L4764;

 L4816: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L553;
    case MINUS:
      goto L579;
    case NEG:
      goto L602;
    case ASHIFT:
      goto L1332;
    case ROTATERT:
      goto L1337;
    case ROTATE:
      goto L1342;
    case NOT:
      goto L1389;
    case IF_THEN_ELSE:
      goto L1544;
    default:
     break;
   }
 L4824: ATTRIBUTE_UNUSED_LABEL
  if (condop_operator (x1, SImode))
    {
      operands[5] = x1;
      goto L1553;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L553: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L4826;
  goto L4824;

 L4826: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L559;
    case MULT:
      goto L566;
    case NOT:
      goto L585;
    case SUBREG:
    case REG:
      goto L4825;
    default:
      goto L4824;
   }
 L4825: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L554;
    }
  goto L4824;

 L559: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L560;
    }
  goto L4824;

 L560: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L561;
    }
  goto L4824;

 L561: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    {
      return 92;
    }
  goto L4824;

 L566: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L567;
    }
  goto L4824;

 L567: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT)
    goto L4829;
  goto L4824;

 L4829: ATTRIBUTE_UNUSED_LABEL
  if (XWINT (x3, 0) == 2LL)
    goto L568;
 L4830: ATTRIBUTE_UNUSED_LABEL
  if (shladd_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L575;
    }
  goto L4824;

 L568: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    {
      return 93;
    }
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 1);
  goto L4830;

 L575: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[3] = x2;
      return 94;
    }
  goto L4824;

 L585: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L586;
    }
  goto L4824;

 L586: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 96;
    }
  goto L4824;

 L554: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_22bit_operand (x2, SImode))
    {
      operands[2] = x2;
      return 91;
    }
  goto L4824;

 L579: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_reg_or_8bit_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L580;
    }
  goto L4824;

 L580: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 95;
    }
  goto L4824;

 L602: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 99;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1332: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1333;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1333: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_5bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 187;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1337: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1338;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1338: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_5bit_operand (x2, DImode))
    {
      operands[2] = x2;
      return 188;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1342: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1343;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1343: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_32bit_count_operand (x2, SImode))
    {
      operands[2] = x2;
      return 189;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1389: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 197;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1544: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L1545;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1545: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1546;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1546: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1547;
  x1 = XEXP (x0, 0);
  goto L4764;

 L1547: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == NEG)
    goto L1548;
  x1 = XEXP (x0, 0);
  goto L4764;

 L1548: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L1549;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1549: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, SImode))
    {
      operands[2] = x2;
      return 220;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1553: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L4831;
  x1 = XEXP (x0, 0);
  goto L4764;

 L4831: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == IF_THEN_ELSE)
    goto L1554;
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L1564;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1554: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L1555;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1555: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L1556;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1556: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1557;
  x1 = XEXP (x0, 0);
  goto L4764;

 L1557: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1558;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1558: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L1559;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1559: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      return 221;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1564: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == IF_THEN_ELSE)
    goto L1565;
  x1 = XEXP (x0, 0);
  goto L4764;

 L1565: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L1566;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1566: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L1567;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1567: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1568;
  x1 = XEXP (x0, 0);
  goto L4764;

 L1568: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1569;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L1569: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      return 222;
    }
  x1 = XEXP (x0, 0);
  goto L4764;

 L589: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L4833;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4833: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case MULT:
      goto L590;
    case PLUS:
      goto L595;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1467;

 L590: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (grfr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L591;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L591: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 97;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L595: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MULT)
    goto L596;
  x1 = XEXP (x0, 0);
  goto L1467;

 L596: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L597;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L597: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L598;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L598: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (grfr_register_operand (x2, SImode))
    {
      operands[3] = x2;
      return 98;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L4760: ATTRIBUTE_UNUSED_LABEL
  tem = recog_2 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  goto L1467;

 L4749: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x1, TImode))
    {
      operands[0] = x1;
      goto L137;
    }
 L4750: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, TImode))
    {
      operands[0] = x1;
      goto L141;
    }
  goto L1467;

 L137: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, TImode))
    {
      operands[1] = x1;
      goto L138;
    }
  x1 = XEXP (x0, 0);
  goto L4750;

 L138: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1]))
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 1;
      return 24;
    }
  x1 = XEXP (x0, 0);
  goto L4750;

 L141: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, TImode))
    {
      operands[1] = x1;
      return 25;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L4751: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L144;
    }
 L4758: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, SFmode))
    {
      operands[0] = x1;
      goto L192;
    }
  goto L1467;

 L144: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, SFmode))
    {
      operands[1] = x1;
      goto L145;
    }
  x1 = XEXP (x0, 0);
  goto L4758;

 L145: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 26;
    }
  x1 = XEXP (x0, 0);
  goto L4758;

 L192: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SFmode)
    goto L4927;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4927: ATTRIBUTE_UNUSED_LABEL
  tem = recog_3 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4752: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L148;
    }
 L4756: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, DFmode))
    {
      operands[0] = x1;
      goto L180;
    }
  goto L1467;

 L148: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, DFmode))
    {
      operands[1] = x1;
      goto L149;
    }
  x1 = XEXP (x0, 0);
  goto L4756;

 L149: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 27;
    }
  x1 = XEXP (x0, 0);
  goto L4756;

 L180: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DFmode)
    goto L4965;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4965: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case FLOAT_EXTEND:
      goto L181;
    case FLOAT_TRUNCATE:
      goto L201;
    case UNSIGNED_FLOAT:
      goto L232;
    case PLUS:
      goto L874;
    case MINUS:
      goto L885;
    case MULT:
      goto L896;
    case ABS:
      goto L907;
    case NEG:
      goto L911;
    case SMIN:
      goto L920;
    case SMAX:
      goto L925;
    case DIV:
      goto L1016;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1467;

 L181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 35;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L201: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L4977;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4977: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L1053;
    case MINUS:
      goto L1070;
    case MULT:
      goto L1087;
    case NEG:
      goto L1222;
    case SUBREG:
    case REG:
      goto L4976;
    default:
      x1 = XEXP (x0, 0);
      goto L1467;
   }
 L4976: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 40;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1053: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L4982;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4982: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1158;
    case NEG:
      goto L1247;
    case REG:
    case CONST_DOUBLE:
      goto L4981;
    default:
      x1 = XEXP (x0, 0);
      goto L1467;
   }
 L4981: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1054;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1158: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1159;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1159: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1160;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1160: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 170;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1247: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L1248;
  x1 = XEXP (x0, 0);
  goto L1467;

 L1248: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1249;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1249: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1250;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1250: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 181;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1054: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 153;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1070: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L4985;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4985: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == MULT)
    goto L1202;
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1071;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1202: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1203;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1203: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1204;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1204: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 175;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1071: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 156;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1087: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1088;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1088: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 159;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1222: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1223;
  x1 = XEXP (x0, 0);
  goto L1467;

 L1223: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1224;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1224: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 178;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L232: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 47;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L874: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L4987;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4987: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L931;
    case NEG:
      goto L974;
    case SUBREG:
    case REG:
      goto L4986;
    default:
      x1 = XEXP (x0, 0);
      goto L1467;
   }
 L4986: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L875;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L931: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L932;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L932: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L933;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L933: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 140;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L974: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L975;
  x1 = XEXP (x0, 0);
  goto L1467;

 L975: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L976;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L976: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L977;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L977: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 146;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L875: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 129;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L885: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L4990;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4990: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L946;
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L886;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L946: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L947;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L947: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L948;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L948: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[3] = x2;
      return 142;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L886: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 131;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L896: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L897;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L897: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 133;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L907: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 135;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L911: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DFmode)
    goto L4992;
  x1 = XEXP (x0, 0);
  goto L1467;

 L4992: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L916;
    case MULT:
      goto L961;
    case SUBREG:
    case REG:
      goto L4991;
    default:
      x1 = XEXP (x0, 0);
      goto L1467;
   }
 L4991: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 136;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L916: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 137;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L961: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L962;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L962: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 144;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L920: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L921;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L921: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 138;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L925: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L926;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L926: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_reg_or_fp01_operand (x2, DFmode))
    {
      operands[2] = x2;
      return 139;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1016: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L1017;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1017: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, DFmode))
    {
      operands[2] = x2;
      goto L1018;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1018: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 4;
      return 149;
    }
 L1037: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 150;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L4753: ATTRIBUTE_UNUSED_LABEL
  if (destination_tfmode_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L152;
    }
 L4757: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L184;
    }
 L4765: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L1729;
    }
 L4766: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, TFmode))
    {
      operands[0] = x1;
      goto L1733;
    }
  goto L1467;

 L152: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_tfmode_operand (x1, TFmode))
    {
      operands[1] = x1;
      goto L153;
    }
  x1 = XEXP (x0, 0);
  goto L4757;

 L153: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && ia64_move_ok (operands[0], operands[1])))
    {
      return 28;
    }
  x1 = XEXP (x0, 0);
  goto L4757;

 L184: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == TFmode)
    goto L4994;
  x1 = XEXP (x0, 0);
  goto L4765;

 L4994: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case FLOAT_EXTEND:
      goto L185;
    case FLOAT:
      goto L205;
    case UNSIGNED_FLOAT:
      goto L236;
    case PLUS:
      goto L1041;
    case MINUS:
      goto L1058;
    case MULT:
      goto L1075;
    case ABS:
      goto L1118;
    case NEG:
      goto L1122;
    case SMIN:
      goto L1131;
    case SMAX:
      goto L1136;
    case DIV:
      goto L1294;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4765;

 L185: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SFmode:
      goto L5005;
    case DFmode:
      goto L5006;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L5005: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[1] = x2;
      return 36;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L5006: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[1] = x2;
      return 37;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L205: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5007;
    case SImode:
      goto L5008;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L5007: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 41;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L5008: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == DIV)
    goto L743;
  x1 = XEXP (x0, 0);
  goto L4765;

 L743: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L744;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L744: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L745;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L745: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 4;
      return 113;
    }
 L766: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 114;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L236: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 48;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1041: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5010;
  x1 = XEXP (x0, 0);
  goto L4765;

 L5010: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L1142;
    case NEG:
      goto L1229;
    case REG:
    case CONST_DOUBLE:
      goto L5009;
    default:
      x1 = XEXP (x0, 0);
      goto L4765;
   }
 L5009: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1042;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1142: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1143;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1143: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1144;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1144: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      return 168;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1229: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1230;
  x1 = XEXP (x0, 0);
  goto L4765;

 L1230: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1231;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1231: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1232;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1232: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      return 179;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1042: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      return 151;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1058: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5013;
  x1 = XEXP (x0, 0);
  goto L4765;

 L5013: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MULT)
    goto L1186;
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1059;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1186: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1187;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1187: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1188;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1188: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[3] = x2;
      return 173;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1059: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      return 154;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1075: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1076;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1076: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      return 157;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1118: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 163;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1122: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5015;
  x1 = XEXP (x0, 0);
  goto L4765;

 L5015: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L1127;
    case MULT:
      goto L1209;
    case REG:
    case CONST_DOUBLE:
      goto L5014;
    default:
      x1 = XEXP (x0, 0);
      goto L4765;
   }
 L5014: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 164;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1127: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 165;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1209: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1210;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1210: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 176;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1131: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1132;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1132: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      return 166;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1136: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1137;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1137: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (tfreg_or_fp01_operand (x2, TFmode))
    {
      operands[2] = x2;
      return 167;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1294: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L1295;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1295: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (fr_register_operand (x2, TFmode))
    {
      operands[2] = x2;
      goto L1296;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1296: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 5;
      return 184;
    }
 L1315: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR)
      && pnum_clobbers != NULL)
    {
      *pnum_clobbers = 3;
      return 185;
    }
  x1 = XEXP (x0, 0);
  goto L4765;

 L1729: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == TFmode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 12)
    goto L1730;
  x1 = XEXP (x0, 0);
  goto L4766;

 L1730: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (register_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 242;
    }
  x1 = XEXP (x0, 0);
  goto L4766;

 L1733: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == TFmode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 13)
    goto L1734;
  x1 = XEXP (x0, 0);
  goto L1467;

 L1734: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (memory_operand (x2, TFmode))
    {
      operands[1] = x2;
      return 243;
    }
  x1 = XEXP (x0, 0);
  goto L1467;

 L1672: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      return 234;
    }
  switch (GET_CODE (x1))
    {
    case IF_THEN_ELSE:
      goto L1469;
    case LABEL_REF:
      goto L1669;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1469: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[0] = x2;
      goto L1470;
    }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1470: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1471;
    }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1471: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1472;
  x1 = XEXP (x0, 0);
  goto L1772;

 L1472: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1473;
    case PC:
      goto L1482;
    case RETURN:
      goto L1657;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1473: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[2] = x3;
  goto L1474;

 L1474: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC)
    {
      return 212;
    }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1482: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  switch (GET_CODE (x2))
    {
    case LABEL_REF:
      goto L1483;
    case RETURN:
      goto L5017;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1483: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[2] = x3;
  return 213;

 L5017: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_direct_return ()))
    {
      return 232;
    }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1657: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (GET_CODE (x2) == PC
      && (ia64_direct_return ()))
    {
      return 231;
    }
  x1 = XEXP (x0, 0);
  goto L1772;

 L1669: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  operands[0] = x2;
  return 233;

 L1773: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BLKmode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 18)
    goto L1774;
  x1 = XEXP (x0, 0);
  goto L1828;

 L1774: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  operands[1] = x2;
  return 261;

 L4767: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L1829;
    }
  goto ret0;

 L1829: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BImode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 15)
    goto L1830;
  goto ret0;

 L1830: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (rtx_equal_p (x2, operands[0]))
    {
      return 268;
    }
  goto ret0;

 L4768: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1837;
    }
  goto ret0;

 L1837: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == UNSPEC
      && XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 24)
    goto L1838;
  goto ret0;

 L1838: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_MODE (x2) == SImode)
    goto L5019;
  goto ret0;

 L5019: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PLUS)
    goto L1843;
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      return 271;
    }
  goto ret0;

 L1843: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode)
    goto L5020;
  goto ret0;

 L5020: ATTRIBUTE_UNUSED_LABEL
  if (basereg_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1844;
    }
 L5021: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L1851;
    }
  goto ret0;

 L1844: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_14bit_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1845;
    }
  x3 = XEXP (x2, 0);
  goto L5021;

 L1845: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 272;
    }
  x1 = XEXP (x0, 1);
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5021;

 L1851: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (basereg_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1852;
    }
  goto ret0;

 L1852: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 273;
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
      goto L5025;
    case DImode:
      goto L5027;
    case BImode:
      goto L5028;
    case DFmode:
      goto L5030;
    case TFmode:
      goto L5031;
    case SFmode:
      goto L5032;
    default:
      break;
    }
 L1486: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PC)
    goto L1487;
 L1579: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L1580;
 L1682: ATTRIBUTE_UNUSED_LABEL
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5033;
    case SImode:
      goto L5035;
    default:
      break;
    }
 L1675: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == PC)
    goto L1676;
  goto ret0;

 L5025: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, TImode))
    {
      operands[0] = x2;
      goto L131;
    }
  goto L1486;

 L131: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, TImode))
    {
      operands[1] = x2;
      goto L132;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L132: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L133;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L133: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L134;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L134: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 24;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L5027: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L286;
  if (fr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L221;
    }
 L5029: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L675;
    }
  goto L1486;

 L286: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L287;
    }
  goto L1486;

 L287: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL)
    goto L288;
  goto L1486;

 L288: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L289;
  goto L1486;

 L289: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L290;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L290: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L291;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L291: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 57;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L221: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5037;
  x2 = XEXP (x1, 0);
  goto L5029;

 L5037: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FIX:
      goto L222;
    case UNSIGNED_FIX:
      goto L253;
    case PLUS:
      goto L658;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5029;

 L222: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L223;
    }
  x2 = XEXP (x1, 0);
  goto L5029;

 L223: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L224;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5029;

 L224: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      return 45;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5029;

 L253: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L254;
    }
  x2 = XEXP (x1, 0);
  goto L5029;

 L254: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L255;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5029;

 L255: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[2] = x2;
      return 52;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5029;

 L658: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L659;
  x2 = XEXP (x1, 0);
  goto L5029;

 L659: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (grfr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L660;
    }
  x2 = XEXP (x1, 0);
  goto L5029;

 L660: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (grfr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L661;
    }
  x2 = XEXP (x1, 0);
  goto L5029;

 L661: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L662;
    }
  x2 = XEXP (x1, 0);
  goto L5029;

 L662: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L663;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5029;

 L663: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      return 107;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5029;

 L675: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L676;
  x2 = XEXP (x1, 0);
  goto L1486;

 L676: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L677;
  x2 = XEXP (x1, 0);
  goto L1486;

 L677: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L678;
  x2 = XEXP (x1, 0);
  goto L1486;

 L678: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L679;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L679: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L680;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L680: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L681;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L681: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[4] = x3;
      goto L682;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L682: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L683;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L683: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[5] = x2;
      goto L684;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L684: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 108;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L5028: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L340;
    }
  goto L1486;

 L340: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == NOT)
    goto L341;
  x2 = XEXP (x1, 0);
  goto L1486;

 L341: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L342;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L342: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L343;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L343: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[2] = x2;
      return 65;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L5030: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L981;
    }
  goto L1486;

 L981: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5040;
  x2 = XEXP (x1, 0);
  goto L1486;

 L5040: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L982;
    case FLOAT_TRUNCATE:
      goto L1110;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1486;

 L982: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == NEG)
    goto L983;
  x2 = XEXP (x1, 0);
  goto L1486;

 L983: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L984;
  x2 = XEXP (x1, 0);
  goto L1486;

 L984: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L985;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L985: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L986;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L986: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      goto L987;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L987: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L988;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L988: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      return 147;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1110: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5042;
  x2 = XEXP (x1, 0);
  goto L1486;

 L5042: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1111;
    case PLUS:
      goto L1176;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1111: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1112;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1112: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1113;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1113: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1114;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1114: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[3] = x2;
      return 162;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1176: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5044;
  x2 = XEXP (x1, 0);
  goto L1486;

 L5044: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L1177;
    case NEG:
      goto L1268;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1177: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1178;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1178: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1179;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1179: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L1180;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1180: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1181;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      return 172;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1268: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L1269;
  x2 = XEXP (x1, 0);
  goto L1486;

 L1269: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L1270;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1270: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L1271;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1271: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L1272;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1272: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1273;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1273: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      return 183;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L5031: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L1092;
    }
  goto L1486;

 L1092: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5046;
  x2 = XEXP (x1, 0);
  goto L1486;

 L5046: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L1093;
    case PLUS:
      goto L1165;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1093: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1094;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1094: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1095;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1095: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1096;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1096: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[3] = x2;
      return 160;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1165: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5048;
  x2 = XEXP (x1, 0);
  goto L1486;

 L5048: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L1166;
    case NEG:
      goto L1256;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1166: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1167;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1167: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1168;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1168: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1169;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1169: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1170;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1170: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      return 171;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1256: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L1257;
  x2 = XEXP (x1, 0);
  goto L1486;

 L1257: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L1258;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1258: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L1259;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1259: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1260;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1260: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1261;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1261: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      return 182;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L5032: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L1100;
    }
  goto L1486;

 L1100: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode
      && GET_CODE (x2) == FLOAT_TRUNCATE)
    goto L1101;
  x2 = XEXP (x1, 0);
  goto L1486;

 L1101: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L1102;
  x2 = XEXP (x1, 0);
  goto L1486;

 L1102: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L1103;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1103: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L1104;
    }
  x2 = XEXP (x1, 0);
  goto L1486;

 L1104: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1105;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1105: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[3] = x2;
      return 161;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1486;

 L1487: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == IF_THEN_ELSE)
    goto L1488;
  x2 = XEXP (x1, 0);
  goto L1579;

 L1488: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == NE)
    goto L1489;
  x2 = XEXP (x1, 0);
  goto L1579;

 L1489: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (ar_lc_reg_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L1490;
    }
  x2 = XEXP (x1, 0);
  goto L1579;

 L1490: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1491;
  x2 = XEXP (x1, 0);
  goto L1579;

 L1491: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == LABEL_REF)
    goto L1492;
  x2 = XEXP (x1, 0);
  goto L1579;

 L1492: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  operands[1] = x4;
  goto L1493;

 L1493: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == PC)
    goto L1494;
  x2 = XEXP (x1, 0);
  goto L1579;

 L1494: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1495;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1495: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[0]))
    goto L1496;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1496: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == IF_THEN_ELSE)
    goto L1497;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1497: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == NE)
    goto L1498;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1498: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (rtx_equal_p (x4, operands[0]))
    goto L1499;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1499: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1500;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1500: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L1501;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1501: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (rtx_equal_p (x4, operands[0]))
    goto L1502;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1502: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == -1LL)
    goto L1503;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1503: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (rtx_equal_p (x3, operands[0]))
    {
      return 214;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1579;

 L1580: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L1581;
  x2 = XEXP (x1, 0);
  goto L1682;

 L1581: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L1582;
  x2 = XEXP (x1, 0);
  goto L1682;

 L1582: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L1583;
    }
  x2 = XEXP (x1, 0);
  goto L1682;

 L1583: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT)
    goto L5050;
  x2 = XEXP (x1, 0);
  goto L1682;

 L5050: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x3, 0) == XWINT (x3, 0))
    switch ((int) XWINT (x3, 0))
      {
      case 0LL:
        goto L1584;
      case 1LL:
        goto L1629;
      default:
        break;
      }
  x2 = XEXP (x1, 0);
  goto L1682;

 L1584: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1585;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1682;

 L1585: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 224;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1682;

 L1629: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1630;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1682;

 L1630: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1631;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1682;

 L1631: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 227;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1682;

 L5033: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1683;
    }
 L5034: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1714;
    }
 L5036: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1823;
    }
  goto L1675;

 L1683: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5052;
  x2 = XEXP (x1, 0);
  goto L5034;

 L5052: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L1684;
    case UNSPEC:
      goto L5055;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5053;
    default:
      x2 = XEXP (x1, 0);
      goto L5034;
   }
 L5053: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1693;
    }
  x2 = XEXP (x1, 0);
  goto L5034;

 L1684: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1685;
    }
  x2 = XEXP (x1, 0);
  goto L5034;

 L1685: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1686;
    }
  x2 = XEXP (x1, 0);
  goto L5034;

 L1686: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1687;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L1687: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1688;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L1688: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (rtx_equal_p (x2, operands[3]))
    {
      return 236;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L5055: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 2
      && XINT (x2, 1) == 11)
    goto L1723;
  x2 = XEXP (x1, 0);
  goto L5034;

 L1723: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1724;
    }
  x2 = XEXP (x1, 0);
  goto L5034;

 L1724: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1725;
    }
  x2 = XEXP (x1, 0);
  goto L5034;

 L1725: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1726;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L1726: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      return 241;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L1693: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1694;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L1694: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[1]))
    goto L1695;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L1695: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (rtx_equal_p (x2, operands[1]))
    {
      return 237;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5034;

 L1714: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 2
      && XINT (x2, 1) == 10)
    goto L1715;
  x2 = XEXP (x1, 0);
  goto L5036;

 L1715: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L1716;
    }
  x2 = XEXP (x1, 0);
  goto L5036;

 L1716: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1717;
    }
  x2 = XEXP (x1, 0);
  goto L5036;

 L1717: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1718;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5036;

 L1718: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[3] = x2;
      return 240;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L5036;

 L1823: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (not_postinc_memory_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1824;
    }
 L1787: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x2, operands[1]))
    goto L1788;
  x2 = XEXP (x1, 0);
  goto L1675;

 L1824: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1825;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1787;

 L1825: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[1]))
    goto L1826;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1787;

 L1826: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, DImode))
    {
      operands[2] = x2;
      return 267;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1787;

 L1788: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1789;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1789: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (not_postinc_memory_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1790;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1790: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5056;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5056: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L5058;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5058: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x2, 0))
    {
    case 2:
      goto L5060;
    case 3:
      goto L5061;
    default:
      break;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5060: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 20)
    goto L1791;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1791: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1792;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1792: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (fetchadd_operand (x3, DImode))
    {
      operands[2] = x3;
      return 263;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5061: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 19)
    goto L1810;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1810: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1811;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1811: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L1812;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1812: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 2);
  if (ar_ccv_reg_operand (x3, DImode))
    {
      operands[3] = x3;
      return 265;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5035: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L1816;
    }
  goto L1675;

 L1816: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (not_postinc_memory_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1817;
    }
 L1778: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x2, operands[1]))
    goto L1779;
  x2 = XEXP (x1, 0);
  goto L1675;

 L1817: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1818;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1778;

 L1818: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, operands[1]))
    goto L1819;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1778;

 L1819: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[2] = x2;
      return 266;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  goto L1778;

 L1779: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1780;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1780: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (not_postinc_memory_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1781;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1781: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L5062;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5062: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == UNSPEC)
    goto L5064;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5064: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x2, 0))
    {
    case 2:
      goto L5066;
    case 3:
      goto L5067;
    default:
      break;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5066: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 20)
    goto L1782;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1782: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1783;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1783: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (fetchadd_operand (x3, SImode))
    {
      operands[2] = x3;
      return 262;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L5067: ATTRIBUTE_UNUSED_LABEL
  if (XINT (x2, 1) == 19)
    goto L1800;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1800: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L1801;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1801: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L1802;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1802: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 2);
  if (ar_ccv_reg_operand (x3, VOIDmode))
    {
      operands[3] = x3;
      return 264;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1675;

 L1676: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1677;
    }
  goto ret0;

 L1677: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1678;
  goto ret0;

 L1678: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == LABEL_REF)
    goto L1679;
  goto ret0;

 L1679: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[1] = x3;
  return 235;
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
      goto L604;
    case 4:
      goto L747;
    case 6:
      goto L1275;
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
      goto L1634;
    default:
     break;
   }
  goto ret0;

 L16: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L5022;
    case DImode:
      goto L5023;
    case TFmode:
      goto L5024;
    default:
      break;
    }
  goto ret0;

 L5022: ATTRIBUTE_UNUSED_LABEL
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

 L5023: ATTRIBUTE_UNUSED_LABEL
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

 L5024: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L1319;
    }
  goto ret0;

 L1319: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == DIV)
    goto L1320;
  goto ret0;

 L1320: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    goto L1321;
  goto ret0;

 L1321: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[3] = x3;
      goto L1322;
    }
  goto ret0;

 L1322: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET)
    goto L1323;
  goto ret0;

 L1323: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1324;
    }
  goto ret0;

 L1324: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 2
      && XINT (x2, 1) == 14)
    goto L1325;
  goto ret0;

 L1325: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1326;
    }
  goto ret0;

 L1326: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (rtx_equal_p (x3, operands[3]))
    goto L1327;
  goto ret0;

 L1327: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1328;
  goto ret0;

 L1328: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, SImode))
    {
      operands[4] = x2;
      return 186;
    }
  goto ret0;

 L1634: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L1635;
  goto ret0;

 L1635: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L1636;
    }
  goto ret0;

 L1636: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    goto L1637;
  goto ret0;

 L1637: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1638;
  goto ret0;

 L1638: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1639;
    }
  goto ret0;

 L1639: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1640;
  goto ret0;

 L1640: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      return 228;
    }
  goto ret0;

 L129: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L130;
    case CALL:
      goto L1572;
    case RETURN:
      goto L1647;
    default:
     break;
   }
  goto ret0;

 L130: ATTRIBUTE_UNUSED_LABEL
  return recog_5 (x0, insn, pnum_clobbers);

 L1572: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L1573;
 L1603: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == MEM)
    goto L1604;
  goto ret0;

 L1573: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L1574;
    }
  goto L1603;

 L1574: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L1575;
  x2 = XEXP (x1, 0);
  goto L1603;

 L1575: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1576;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1603;

 L1576: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      return 223;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1603;

 L1604: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L1605;
    }
  goto ret0;

 L1605: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    goto L1606;
  goto ret0;

 L1606: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1607;
  goto ret0;

 L1607: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1608;
    }
  goto ret0;

 L1608: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 226;
    }
  goto ret0;

 L1647: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1648;
  goto ret0;

 L1648: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      return 229;
    }
  goto ret0;

 L604: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L605;
  goto ret0;

 L605: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5068;
    case DFmode:
      goto L5069;
    case DImode:
      goto L5070;
    default:
      break;
    }
  goto ret0;

 L5068: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L606;
    }
  goto ret0;

 L606: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == FLOAT)
    goto L607;
  goto ret0;

 L607: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L608;
  goto ret0;

 L608: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L609;
    }
  goto ret0;

 L609: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L610;
    }
  goto ret0;

 L610: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L611;
  goto ret0;

 L611: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5071;
  goto ret0;

 L5071: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L612;
    }
 L5072: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L732;
    }
  goto ret0;

 L612: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L613;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5072;

 L613: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L614;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5072;

 L614: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L615;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5072;

 L615: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L616;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5072;

 L616: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == USE)
    goto L617;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5072;

 L617: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L618;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5072;

 L618: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV))
    {
      return 100;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5072;

 L732: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L733;
  goto ret0;

 L733: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L734;
    }
  goto ret0;

 L734: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L735;
  goto ret0;

 L735: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L736;
    }
  goto ret0;

 L736: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L737;
  goto ret0;

 L737: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L738;
    }
  goto ret0;

 L738: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT))
    {
      return 113;
    }
  goto ret0;

 L5069: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L1001;
    }
  goto ret0;

 L1001: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L1002;
  goto ret0;

 L1002: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L1003;
    }
  goto ret0;

 L1003: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L1004;
    }
  goto ret0;

 L1004: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1005;
  goto ret0;

 L1005: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1006;
    }
  goto ret0;

 L1006: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1007;
  goto ret0;

 L1007: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L1008;
    }
  goto ret0;

 L1008: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1009;
  goto ret0;

 L1009: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L1010;
    }
  goto ret0;

 L1010: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L1011;
  goto ret0;

 L1011: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L1012;
    }
  goto ret0;

 L1012: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT))
    {
      return 149;
    }
  goto ret0;

 L5070: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1701;
    }
  goto ret0;

 L1701: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC_VOLATILE
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 0)
    goto L1702;
  goto ret0;

 L1702: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1703;
  goto ret0;

 L1703: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE)
    goto L1704;
  goto ret0;

 L1704: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1705;
    }
  goto ret0;

 L1705: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1706;
  goto ret0;

 L1706: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1707;
    }
  goto ret0;

 L1707: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == USE)
    goto L1708;
  goto ret0;

 L1708: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1709;
    }
  goto ret0;

 L1709: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == USE)
    goto L1710;
  goto ret0;

 L1710: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (const_int_operand (x2, DImode))
    {
      operands[4] = x2;
      return 239;
    }
  goto ret0;

 L747: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L748;
    case CALL:
      goto L1592;
    default:
     break;
   }
  goto ret0;

 L748: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5073;
    case SFmode:
      goto L5074;
    case DFmode:
      goto L5075;
    default:
      break;
    }
 L1611: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L1612;

 L5073: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L749;
    }
  goto L1611;

 L749: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5076;
  x2 = XEXP (x1, 0);
  goto L1611;

 L5076: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT:
      goto L750;
    case DIV:
      goto L1301;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L1611;

 L750: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L751;
  x2 = XEXP (x1, 0);
  goto L1611;

 L751: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L752;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L752: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L753;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L753: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L754;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L754: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L755;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L755: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L756;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L756: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L757;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L757: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L758;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L758: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L759;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L759: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR))
    {
      return 114;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1301: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1302;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L1302: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1303;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L1303: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1304;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1304: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1305;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1305: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1306;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1306: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L1307;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1307: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1308;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1308: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L1309;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1309: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR))
    {
      return 185;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L5074: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L836;
    }
  goto L1611;

 L836: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode
      && GET_CODE (x2) == DIV)
    goto L837;
  x2 = XEXP (x1, 0);
  goto L1611;

 L837: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L838;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L838: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L839;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L839: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L840;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L840: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L841;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L841: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L842;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L842: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L843;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L843: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L844;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L844: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L845;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L845: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT))
    {
      return 127;
    }
 L864: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR))
    {
      return 128;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L5075: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L1022;
    }
  goto L1611;

 L1022: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L1023;
  x2 = XEXP (x1, 0);
  goto L1611;

 L1023: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L1024;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L1024: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L1025;
    }
  x2 = XEXP (x1, 0);
  goto L1611;

 L1025: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1026;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1026: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1027;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1027: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1028;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1028: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      operands[4] = x2;
      goto L1029;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1029: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1030;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1030: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L1031;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1031: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR))
    {
      return 150;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1611;

 L1612: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L1613;
  goto ret0;

 L1613: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L1614;
  goto ret0;

 L1614: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L1615;
    }
  goto ret0;

 L1615: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    goto L1616;
  goto ret0;

 L1616: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1617;
  goto ret0;

 L1617: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1618;
    }
  goto ret0;

 L1618: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1619;
  goto ret0;

 L1619: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L1620;
    }
  goto ret0;

 L1620: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1621;
  goto ret0;

 L1621: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      return 227;
    }
  goto ret0;

 L1592: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == MEM)
    goto L1593;
  goto ret0;

 L1593: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L1594;
    }
  goto ret0;

 L1594: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    goto L1595;
  goto ret0;

 L1595: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1596;
  goto ret0;

 L1596: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1597;
    }
  goto ret0;

 L1597: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1598;
  goto ret0;

 L1598: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1599;
    }
  goto ret0;

 L1599: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1600;
  goto ret0;

 L1600: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      return 226;
    }
  goto ret0;

 L1275: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L1276;
  goto ret0;

 L1276: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L1277;
    }
  goto ret0;

 L1277: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == DIV)
    goto L1278;
  goto ret0;

 L1278: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L1279;
    }
  goto ret0;

 L1279: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L1280;
    }
  goto ret0;

 L1280: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1281;
  goto ret0;

 L1281: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L1282;
    }
  goto ret0;

 L1282: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L1283;
  goto ret0;

 L1283: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L1284;
    }
  goto ret0;

 L1284: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L1285;
  goto ret0;

 L1285: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L1286;
    }
  goto ret0;

 L1286: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L1287;
  goto ret0;

 L1287: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[6] = x2;
      goto L1288;
    }
  goto ret0;

 L1288: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == CLOBBER)
    goto L1289;
  goto ret0;

 L1289: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[7] = x2;
      goto L1290;
    }
  goto ret0;

 L1290: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT))
    {
      return 184;
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
      goto L5097;
    case QImode:
      goto L5098;
    case HImode:
      goto L5099;
    case SImode:
      goto L5100;
    case DImode:
      goto L5112;
    case SFmode:
      goto L5103;
    case DFmode:
      goto L5104;
    case TFmode:
      goto L5105;
    default:
      break;
    }
  goto ret0;

 L5097: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2321;
    }
  goto ret0;

 L2321: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, BImode))
    {
      operands[1] = x2;
      return 418;
    }
  goto ret0;

 L5098: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, QImode))
    {
      operands[0] = x2;
      goto L2328;
    }
  goto ret0;

 L2328: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, QImode))
    {
      operands[1] = x2;
      goto L2329;
    }
  goto ret0;

 L2329: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 419;
    }
  goto ret0;

 L5099: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, HImode))
    {
      operands[0] = x2;
      goto L2336;
    }
  goto ret0;

 L2336: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, HImode))
    {
      operands[1] = x2;
      goto L2337;
    }
  goto ret0;

 L2337: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 420;
    }
  goto ret0;

 L5100: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L2356;
    }
  goto ret0;

 L2356: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2357;
    }
  goto ret0;

 L2357: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 422;
    }
  goto ret0;

 L5112: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L2816;
  if (destination_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2376;
    }
 L5102: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2384;
    }
 L5106: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2562;
    }
 L5107: ATTRIBUTE_UNUSED_LABEL
  if (grfr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2578;
    }
 L5111: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2666;
    }
  goto ret0;

 L2816: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2817;
    }
  goto ret0;

 L2817: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL)
    goto L2818;
  goto ret0;

 L2818: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2819;
  goto ret0;

 L2819: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == LSHIFTRT)
    goto L2820;
  goto ret0;

 L2820: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2821;
    }
  goto ret0;

 L2821: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL)
    {
      return 473;
    }
  goto ret0;

 L2376: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2377;
    }
  x2 = XEXP (x1, 0);
  goto L5102;

 L2377: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 424;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5102;

 L2384: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5113;
  x2 = XEXP (x1, 0);
  goto L5106;

 L5113: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2385;
    case MINUS:
      goto L2403;
    case UNSPEC:
      goto L5117;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5106;

 L2385: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    goto L2432;
  x2 = XEXP (x1, 0);
  goto L5106;

 L2432: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5119;
 L2386: ATTRIBUTE_UNUSED_LABEL
  if (function_operand (x3, VOIDmode))
    {
      operands[1] = x3;
      return 425;
    }
 L2395: ATTRIBUTE_UNUSED_LABEL
  if (sdata_symbolic_operand (x3, VOIDmode))
    {
      operands[1] = x3;
      return 426;
    }
  x2 = XEXP (x1, 0);
  goto L5106;

 L5119: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == UNSPEC)
    goto L5122;
  goto L2386;

 L5122: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x3, 0) == 1)
    goto L5125;
  goto L2386;

 L5125: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x3, 1))
    {
    case 0LL:
      goto L2433;
    case 1LL:
      goto L2443;
    case 3LL:
      goto L2492;
    default:
      break;
    }
  goto L2386;

 L2433: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[1] = x4;
      return 430;
    }
  goto L2386;

 L2443: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[1] = x4;
      return 431;
    }
  goto L2386;

 L2492: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[1] = x4;
      return 436;
    }
  goto L2386;

 L2403: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2404;
    }
  x2 = XEXP (x1, 0);
  goto L5106;

 L2404: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    {
      return 427;
    }
  x2 = XEXP (x1, 0);
  goto L5106;

 L5117: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1)
    goto L5128;
  x2 = XEXP (x1, 0);
  goto L5106;

 L5128: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x2, 1))
    {
    case 2LL:
      goto L2451;
    case 4LL:
      goto L2500;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5106;

 L2451: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_MODE (x3) == DImode)
    goto L5130;
  x2 = XEXP (x1, 0);
  goto L5106;

 L5130: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2452;
    }
 L5131: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      return 433;
    }
  x2 = XEXP (x1, 0);
  goto L5106;

 L2452: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 432;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  x3 = XVECEXP (x2, 0, 0);
  goto L5131;

 L2500: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_MODE (x3) == DImode)
    goto L5132;
  x2 = XEXP (x1, 0);
  goto L5106;

 L5132: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2501;
    }
 L5133: ATTRIBUTE_UNUSED_LABEL
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      return 438;
    }
  x2 = XEXP (x1, 0);
  goto L5106;

 L2501: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS64))
    {
      return 437;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  x3 = XVECEXP (x2, 0, 0);
  goto L5133;

 L2562: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5134;
  x2 = XEXP (x1, 0);
  goto L5107;

 L5134: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case SIGN_EXTEND:
      goto L2563;
    case ZERO_EXTEND:
      goto L2587;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5107;

 L2563: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case QImode:
      goto L5136;
    case HImode:
      goto L5137;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5107;

 L5136: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, QImode))
    {
      operands[1] = x3;
      return 444;
    }
  x2 = XEXP (x1, 0);
  goto L5107;

 L5137: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, HImode))
    {
      operands[1] = x3;
      return 445;
    }
  x2 = XEXP (x1, 0);
  goto L5107;

 L2587: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case QImode:
      goto L5138;
    case HImode:
      goto L5139;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5107;

 L5138: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x3, QImode))
    {
      operands[1] = x3;
      return 447;
    }
  x2 = XEXP (x1, 0);
  goto L5107;

 L5139: ATTRIBUTE_UNUSED_LABEL
  if (gr_nonimmediate_operand (x3, HImode))
    {
      operands[1] = x3;
      return 448;
    }
  x2 = XEXP (x1, 0);
  goto L5107;

 L2578: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5140;
  x2 = XEXP (x1, 0);
  goto L5111;

 L5140: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case SIGN_EXTEND:
      goto L2579;
    case ZERO_EXTEND:
      goto L2603;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5111;

 L2579: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 446;
    }
  x2 = XEXP (x1, 0);
  goto L5111;

 L2603: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_nonimmediate_operand (x3, SImode))
    {
      operands[1] = x3;
      return 449;
    }
  x2 = XEXP (x1, 0);
  goto L5111;

 L2666: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5142;
  goto ret0;

 L5142: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FIX:
      goto L2667;
    case UNSIGNED_FIX:
      goto L2726;
    default:
     break;
   }
  goto ret0;

 L2667: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5144;
    case DFmode:
      goto L5145;
    case TFmode:
      goto L5146;
    default:
      break;
    }
  goto ret0;

 L5144: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 457;
    }
  goto ret0;

 L5145: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 458;
    }
  goto ret0;

 L5146: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 459;
    }
  goto ret0;

 L2726: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5147;
    case DFmode:
      goto L5148;
    case TFmode:
      goto L5149;
    default:
      break;
    }
  goto ret0;

 L5147: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 464;
    }
  goto ret0;

 L5148: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 465;
    }
  goto ret0;

 L5149: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 466;
    }
  goto ret0;

 L5103: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L2538;
    }
 L5110: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L2634;
    }
  goto ret0;

 L2538: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, SFmode))
    {
      operands[1] = x2;
      goto L2539;
    }
  x2 = XEXP (x1, 0);
  goto L5110;

 L2539: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 441;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5110;

 L2634: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5150;
  goto ret0;

 L5150: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT_TRUNCATE:
      goto L2635;
    case UNSIGNED_FLOAT:
      goto L2702;
    default:
     break;
   }
  goto ret0;

 L2635: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DFmode:
      goto L5152;
    case TFmode:
      goto L5153;
    default:
      break;
    }
  goto ret0;

 L5152: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 453;
    }
  goto ret0;

 L5153: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 454;
    }
  goto ret0;

 L2702: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 461;
    }
  goto ret0;

 L5104: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2546;
    }
 L5108: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2610;
    }
  goto ret0;

 L2546: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, DFmode))
    {
      operands[1] = x2;
      goto L2547;
    }
  x2 = XEXP (x1, 0);
  goto L5108;

 L2547: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_move_ok (operands[0], operands[1])))
    {
      return 442;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5108;

 L2610: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5154;
  goto ret0;

 L5154: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT_EXTEND:
      goto L2611;
    case FLOAT_TRUNCATE:
      goto L2651;
    case UNSIGNED_FLOAT:
      goto L2710;
    default:
     break;
   }
  goto ret0;

 L2611: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 450;
    }
  goto ret0;

 L2651: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 455;
    }
  goto ret0;

 L2710: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 462;
    }
  goto ret0;

 L5105: ATTRIBUTE_UNUSED_LABEL
  if (destination_tfmode_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2554;
    }
 L5109: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2618;
    }
  goto ret0;

 L2554: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_tfmode_operand (x2, TFmode))
    {
      operands[1] = x2;
      goto L2555;
    }
  x2 = XEXP (x1, 0);
  goto L5109;

 L2555: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && ia64_move_ok (operands[0], operands[1])))
    {
      return 443;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5109;

 L2618: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5157;
  goto ret0;

 L5157: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT_EXTEND:
      goto L2619;
    case FLOAT:
      goto L2659;
    case UNSIGNED_FLOAT:
      goto L2718;
    default:
     break;
   }
  goto ret0;

 L2619: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5160;
    case DFmode:
      goto L5161;
    default:
      break;
    }
  goto ret0;

 L5160: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 451;
    }
  goto ret0;

 L5161: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 452;
    }
  goto ret0;

 L2659: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 456;
    }
  goto ret0;

 L2718: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 463;
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
      goto L5171;
    case BImode:
      goto L5173;
    case HImode:
      goto L5174;
    case SImode:
      goto L5175;
    case SFmode:
      goto L5178;
    case DFmode:
      goto L5179;
    case TFmode:
      goto L5180;
    default:
      break;
    }
  goto ret0;

 L5171: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2411;
    }
 L5172: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2838;
    }
 L5177: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L3340;
    }
 L5181: ATTRIBUTE_UNUSED_LABEL
  if (grfr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4218;
    }
  goto ret0;

 L2411: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5182;
  x2 = XEXP (x1, 0);
  goto L5172;

 L5182: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L2412;
    case LO_SUM:
      goto L2422;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2412: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L5184;
  x2 = XEXP (x1, 0);
  goto L5172;

 L5184: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == HIGH)
    goto L2413;
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2469;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2413: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (got_symbolic_operand (x4, VOIDmode))
    {
      operands[1] = x4;
      goto L2414;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2414: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 428;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2469: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5186;
  x2 = XEXP (x1, 0);
  goto L5172;

 L5186: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == UNSPEC)
    goto L5188;
  x2 = XEXP (x1, 0);
  goto L5172;

 L5188: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x3, 0) == 1)
    goto L5190;
  x2 = XEXP (x1, 0);
  goto L5172;

 L5190: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x3, 1))
    {
    case 2LL:
      goto L2470;
    case 4LL:
      goto L2519;
    default:
      break;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2470: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2471;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2471: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS14))
    {
      return 434;
    }
 L2482: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS22))
    {
      return 435;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5172;

 L2519: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (symbolic_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2520;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2520: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS14))
    {
      return 439;
    }
 L2531: ATTRIBUTE_UNUSED_LABEL
  if ((TARGET_TLS22))
    {
      return 440;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 0);
  goto L5172;

 L2422: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2423;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2423: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (got_symbolic_operand (x3, VOIDmode))
    {
      operands[2] = x3;
      return 429;
    }
  x2 = XEXP (x1, 0);
  goto L5172;

 L2838: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5192;
  x2 = XEXP (x1, 0);
  goto L5177;

 L5192: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case IOR:
      goto L2839;
    case PLUS:
      goto L3291;
    case MINUS:
      goto L3322;
    case ASHIFT:
      goto L4141;
    case ASHIFTRT:
      goto L4175;
    case LSHIFTRT:
      goto L4184;
    case ROTATERT:
      goto L4193;
    case ROTATE:
      goto L4202;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5177;

 L2839: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTEND)
    goto L2840;
  x2 = XEXP (x1, 0);
  goto L5177;

 L2840: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L2841;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L2841: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ASHIFT)
    goto L2842;
  x2 = XEXP (x1, 0);
  goto L5177;

 L2842: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTEND)
    goto L2843;
  x2 = XEXP (x1, 0);
  goto L5177;

 L2843: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L2844;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L2844: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 32LL)
    {
      return 475;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3291: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L5201;
  x2 = XEXP (x1, 0);
  goto L5177;

 L5201: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3301;
    case NOT:
      goto L3332;
    case SUBREG:
    case REG:
      goto L5200;
    default:
      x2 = XEXP (x1, 0);
      goto L5177;
   }
 L5200: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3292;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3301: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L3302;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3302: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3303;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3303: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    {
      return 516;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3332: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L3333;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3333: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 519;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3292: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 515;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3322: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3323;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3323: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 518;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4141: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4142;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4142: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_6bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 595;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4175: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4176;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4176: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_6bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 598;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4184: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4185;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4185: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_6bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 599;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4193: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4194;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4194: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (shift_count_operand (x3, DImode))
    {
      operands[2] = x3;
      return 600;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4202: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4203;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L4203: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (shift_count_operand (x3, DImode))
    {
      operands[2] = x3;
      return 601;
    }
  x2 = XEXP (x1, 0);
  goto L5177;

 L3340: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5203;
  x2 = XEXP (x1, 0);
  goto L5181;

 L5203: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case MULT:
      goto L3341;
    case TRUNCATE:
      goto L3381;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3341: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L3342;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3342: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 520;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3381: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode
      && GET_CODE (x3) == LSHIFTRT)
    goto L3382;
  x2 = XEXP (x1, 0);
  goto L5181;

 L3382: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TImode
      && GET_CODE (x4) == MULT)
    goto L3383;
  x2 = XEXP (x1, 0);
  goto L5181;

 L3383: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TImode)
    goto L5205;
  x2 = XEXP (x1, 0);
  goto L5181;

 L5205: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x5))
    {
    case SIGN_EXTEND:
      goto L3384;
    case ZERO_EXTEND:
      goto L3398;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3384: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L3385;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3385: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode
      && GET_CODE (x5) == SIGN_EXTEND)
    goto L3386;
  x2 = XEXP (x1, 0);
  goto L5181;

 L3386: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L3387;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3387: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 64LL)
    {
      return 523;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3398: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L3399;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3399: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode
      && GET_CODE (x5) == ZERO_EXTEND)
    goto L3400;
  x2 = XEXP (x1, 0);
  goto L5181;

 L3400: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L3401;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L3401: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 64LL)
    {
      return 524;
    }
  x2 = XEXP (x1, 0);
  goto L5181;

 L4218: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5207;
  goto ret0;

 L5207: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L4219;
    case IOR:
      goto L4238;
    case XOR:
      goto L4247;
    default:
     break;
   }
  goto ret0;

 L4219: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode)
    goto L5211;
  goto ret0;

 L5211: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == NOT)
    goto L4229;
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4220;
    }
  goto ret0;

 L4229: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (grfr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4230;
    }
  goto ret0;

 L4230: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 604;
    }
  goto ret0;

 L4220: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 603;
    }
  goto ret0;

 L4238: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4239;
    }
  goto ret0;

 L4239: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 605;
    }
  goto ret0;

 L4247: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4248;
    }
  goto ret0;

 L4248: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_reg_or_8bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 606;
    }
  goto ret0;

 L5173: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2851;
    }
  goto ret0;

 L2851: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode)
    goto L5212;
  goto ret0;

 L5212: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2852;
    case IOR:
      goto L2871;
    case EQ:
      goto L4327;
    case NE:
      goto L4339;
    default:
     break;
   }
  goto ret0;

 L2852: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5217;
  goto ret0;

 L5217: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L2862;
    case NE:
      goto L2994;
    case EQ:
      goto L3007;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5216;
    default:
      goto ret0;
   }
 L5216: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2853;
    }
  goto ret0;

 L2862: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2863;
    }
  goto ret0;

 L2863: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 477;
    }
  goto ret0;

 L2994: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L2995;
  goto ret0;

 L2995: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L2996;
    }
  goto ret0;

 L2996: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L2997;
  goto ret0;

 L2997: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L2998;
  goto ret0;

 L2998: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 489;
    }
  goto ret0;

 L3007: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L3008;
  goto ret0;

 L3008: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3009;
    }
  goto ret0;

 L3009: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L3010;
  goto ret0;

 L3010: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3011;
  goto ret0;

 L3011: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 490;
    }
  goto ret0;

 L2853: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 476;
    }
  goto ret0;

 L2871: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5221;
  goto ret0;

 L5221: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L2881;
    case NE:
      goto L3140;
    case EQ:
      goto L3153;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5220;
    default:
      goto ret0;
   }
 L5220: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2872;
    }
  goto ret0;

 L2881: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2882;
    }
  goto ret0;

 L2882: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 479;
    }
  goto ret0;

 L3140: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L3141;
  goto ret0;

 L3141: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3142;
    }
  goto ret0;

 L3142: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L3143;
  goto ret0;

 L3143: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3144;
  goto ret0;

 L3144: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 501;
    }
  goto ret0;

 L3153: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == AND)
    goto L3154;
  goto ret0;

 L3154: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3155;
    }
  goto ret0;

 L3155: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L3156;
  goto ret0;

 L3156: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3157;
  goto ret0;

 L3157: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 502;
    }
  goto ret0;

 L2872: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[2] = x3;
      return 478;
    }
  goto ret0;

 L4327: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTRACT)
    goto L4328;
  goto ret0;

 L4328: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4329;
    }
  goto ret0;

 L4329: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L4330;
  goto ret0;

 L4330: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (immediate_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4331;
    }
  goto ret0;

 L4331: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    {
      return 615;
    }
  goto ret0;

 L4339: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ZERO_EXTRACT)
    goto L4340;
  goto ret0;

 L4340: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4341;
    }
  goto ret0;

 L4341: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L4342;
  goto ret0;

 L4342: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (immediate_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4343;
    }
  goto ret0;

 L4343: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    {
      return 616;
    }
  goto ret0;

 L5174: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, HImode))
    {
      operands[0] = x2;
      goto L3192;
    }
  goto ret0;

 L3192: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == HImode
      && GET_CODE (x2) == MULT)
    goto L3193;
  goto ret0;

 L3193: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, HImode))
    {
      operands[1] = x3;
      goto L3194;
    }
  goto ret0;

 L3194: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, HImode))
    {
      operands[2] = x3;
      return 505;
    }
  goto ret0;

 L5175: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3201;
    }
 L5176: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3262;
    }
  goto ret0;

 L3201: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L5224;
  x2 = XEXP (x1, 0);
  goto L5176;

 L5224: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3202;
    case MINUS:
      goto L3244;
    case ASHIFT:
      goto L4114;
    case ROTATERT:
      goto L4123;
    case ROTATE:
      goto L4132;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3202: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode)
    goto L5230;
  x2 = XEXP (x1, 0);
  goto L5176;

 L5230: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3212;
    case NOT:
      goto L3254;
    case SUBREG:
    case REG:
      goto L5229;
    default:
      x2 = XEXP (x1, 0);
      goto L5176;
   }
 L5229: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3203;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3212: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3213;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3213: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3214;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3214: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    {
      return 507;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3254: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3255;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3255: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 511;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3203: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_22bit_operand (x3, SImode))
    {
      operands[2] = x3;
      return 506;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3244: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3245;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3245: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 510;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L4114: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4115;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L4115: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_5bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 592;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L4123: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4124;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L4124: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_5bit_operand (x3, DImode))
    {
      operands[2] = x3;
      return 593;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L4132: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4133;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L4133: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (shift_32bit_count_operand (x3, SImode))
    {
      operands[2] = x3;
      return 594;
    }
  x2 = XEXP (x1, 0);
  goto L5176;

 L3262: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == MULT)
    goto L3263;
  goto ret0;

 L3263: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (grfr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L3264;
    }
  goto ret0;

 L3264: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 512;
    }
  goto ret0;

 L5178: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L3424;
    }
  goto ret0;

 L3424: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5232;
  goto ret0;

 L5232: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3425;
    case MINUS:
      goto L3434;
    case MULT:
      goto L3443;
    case SMIN:
      goto L3477;
    case SMAX:
      goto L3486;
    case NEG:
      goto L3517;
    case FLOAT_TRUNCATE:
      goto L3548;
    default:
     break;
   }
  goto ret0;

 L3425: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3426;
    }
  goto ret0;

 L3426: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 527;
    }
  goto ret0;

 L3434: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3435;
    }
  goto ret0;

 L3435: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 528;
    }
  goto ret0;

 L3443: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3444;
    }
  goto ret0;

 L3444: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 529;
    }
  goto ret0;

 L3477: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3478;
    }
  goto ret0;

 L3478: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 533;
    }
  goto ret0;

 L3486: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L3487;
    }
  goto ret0;

 L3487: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      return 534;
    }
  goto ret0;

 L3517: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == MULT)
    goto L3518;
  goto ret0;

 L3518: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L3519;
    }
  goto ret0;

 L3519: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      return 537;
    }
  goto ret0;

 L3548: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DFmode:
      goto L5239;
    case TFmode:
      goto L5243;
    default:
      break;
    }
  goto ret0;

 L5239: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3549;
    case MINUS:
      goto L3568;
    case MULT:
      goto L3587;
    case NEG:
      goto L3696;
    default:
     break;
   }
  goto ret0;

 L3549: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3550;
    }
  goto ret0;

 L3550: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 540;
    }
  goto ret0;

 L3568: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3569;
    }
  goto ret0;

 L3569: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 542;
    }
  goto ret0;

 L3587: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3588;
    }
  goto ret0;

 L3588: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 544;
    }
  goto ret0;

 L3696: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L3697;
  goto ret0;

 L3697: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3698;
    }
  goto ret0;

 L3698: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      return 555;
    }
  goto ret0;

 L5243: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3756;
    case MINUS:
      goto L3785;
    case MULT:
      goto L3814;
    case NEG:
      goto L4024;
    default:
     break;
   }
  goto ret0;

 L3756: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3757;
    }
  goto ret0;

 L3757: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 560;
    }
  goto ret0;

 L3785: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3786;
    }
  goto ret0;

 L3786: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 563;
    }
  goto ret0;

 L3814: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3815;
    }
  goto ret0;

 L3815: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 566;
    }
  goto ret0;

 L4024: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4025;
  goto ret0;

 L4025: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4026;
    }
  goto ret0;

 L4026: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      return 585;
    }
  goto ret0;

 L5179: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L3538;
    }
  goto ret0;

 L3538: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5247;
  goto ret0;

 L5247: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3539;
    case MINUS:
      goto L3558;
    case MULT:
      goto L3577;
    case SMIN:
      goto L3621;
    case SMAX:
      goto L3630;
    case NEG:
      goto L3685;
    case FLOAT_TRUNCATE:
      goto L3765;
    default:
     break;
   }
  goto ret0;

 L3539: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3540;
    }
  goto ret0;

 L3540: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 539;
    }
  goto ret0;

 L3558: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3559;
    }
  goto ret0;

 L3559: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 541;
    }
  goto ret0;

 L3577: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3578;
    }
  goto ret0;

 L3578: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 543;
    }
  goto ret0;

 L3621: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3622;
    }
  goto ret0;

 L3622: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 548;
    }
  goto ret0;

 L3630: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L3631;
    }
  goto ret0;

 L3631: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      return 549;
    }
  goto ret0;

 L3685: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L3686;
  goto ret0;

 L3686: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3687;
    }
  goto ret0;

 L3687: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      return 554;
    }
  goto ret0;

 L3765: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5254;
  goto ret0;

 L5254: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3766;
    case MINUS:
      goto L3795;
    case MULT:
      goto L3824;
    case NEG:
      goto L4035;
    default:
     break;
   }
  goto ret0;

 L3766: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3767;
    }
  goto ret0;

 L3767: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 561;
    }
  goto ret0;

 L3795: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3796;
    }
  goto ret0;

 L3796: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 564;
    }
  goto ret0;

 L3824: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3825;
    }
  goto ret0;

 L3825: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 567;
    }
  goto ret0;

 L4035: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4036;
  goto ret0;

 L4036: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4037;
    }
  goto ret0;

 L4037: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      return 586;
    }
  goto ret0;

 L5180: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L3745;
    }
  goto ret0;

 L3745: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5258;
  goto ret0;

 L5258: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3746;
    case MINUS:
      goto L3775;
    case MULT:
      goto L3804;
    case SMIN:
      goto L3896;
    case SMAX:
      goto L3905;
    case NEG:
      goto L4013;
    default:
     break;
   }
  goto ret0;

 L3746: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3747;
    }
  goto ret0;

 L3747: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 559;
    }
  goto ret0;

 L3775: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3776;
    }
  goto ret0;

 L3776: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 562;
    }
  goto ret0;

 L3804: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3805;
    }
  goto ret0;

 L3805: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 565;
    }
  goto ret0;

 L3896: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3897;
    }
  goto ret0;

 L3897: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 574;
    }
  goto ret0;

 L3905: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L3906;
    }
  goto ret0;

 L3906: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      return 575;
    }
  goto ret0;

 L4013: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L4014;
  goto ret0;

 L4014: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L4015;
    }
  goto ret0;

 L4015: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      return 584;
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
      goto L5267;
    case BImode:
      goto L5268;
    case SImode:
      goto L5269;
    case SFmode:
      goto L5271;
    case DFmode:
      goto L5272;
    case TFmode:
      goto L5273;
    default:
      break;
    }
  goto ret0;

 L5267: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L2780;
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2760;
    }
  goto ret0;

 L2780: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2781;
    }
  goto ret0;

 L2781: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2782;
    }
  goto ret0;

 L2782: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2783;
    }
  goto ret0;

 L2783: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2784;
    }
  goto ret0;

 L2784: ATTRIBUTE_UNUSED_LABEL
  if (((gr_register_operand (operands[3], DImode) && INTVAL (operands[1]) <= 16)
   || operands[3] == const0_rtx || operands[3] == constm1_rtx))
    {
      return 470;
    }
  goto ret0;

 L2760: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5274;
  goto ret0;

 L5274: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case SIGN_EXTRACT:
      goto L2761;
    case ZERO_EXTRACT:
      goto L2771;
    case AND:
      goto L2792;
    case PLUS:
      goto L4150;
    default:
     break;
   }
  goto ret0;

 L2761: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2762;
    }
  goto ret0;

 L2762: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2763;
    }
  goto ret0;

 L2763: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (const_int_operand (x3, DImode))
    {
      operands[3] = x3;
      return 468;
    }
  goto ret0;

 L2771: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2772;
    }
  goto ret0;

 L2772: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2773;
    }
  goto ret0;

 L2773: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (const_int_operand (x3, DImode))
    {
      operands[3] = x3;
      return 469;
    }
  goto ret0;

 L2792: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == ASHIFT)
    goto L2793;
  goto ret0;

 L2793: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L2794;
    }
  goto ret0;

 L2794: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2795;
    }
  goto ret0;

 L2795: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (const_int_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2796;
    }
  goto ret0;

 L2796: ATTRIBUTE_UNUSED_LABEL
  if ((CONST_OK_FOR_M (INTVAL (operands[2]))
   && ia64_depz_field_mask (operands[3], operands[2]) > 0))
    {
      return 471;
    }
  goto ret0;

 L4150: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L4151;
  goto ret0;

 L4151: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4152;
    }
  goto ret0;

 L4152: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4153;
    }
  goto ret0;

 L4153: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[3] = x3;
      return 596;
    }
  goto ret0;

 L5268: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2911;
    }
  goto ret0;

 L2911: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode)
    goto L5278;
  goto ret0;

 L5278: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2912;
    case IOR:
      goto L3058;
    case EQ:
    case NE:
    case GT:
    case LE:
    case GTU:
    case LEU:
      goto L5280;
    case LT:
    case GE:
    case LTU:
    case GEU:
      goto L5281;
    default:
      goto L5282;
   }
 L5280: ATTRIBUTE_UNUSED_LABEL
  if (normal_comparison_operator (x2, BImode))
    {
      operands[1] = x2;
      goto L4264;
    }
 L5281: ATTRIBUTE_UNUSED_LABEL
  if (adjusted_comparison_operator (x2, BImode))
    {
      operands[1] = x2;
      goto L4273;
    }
 L5282: ATTRIBUTE_UNUSED_LABEL
  if (comparison_operator (x2, BImode))
    {
      operands[1] = x2;
      goto L4300;
    }
  goto ret0;

 L2912: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5284;
  goto ret0;

 L5284: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L2936;
    case NE:
      goto L3020;
    case EQ:
      goto L3034;
    case GE:
    case GT:
    case LE:
    case LT:
      goto L5283;
    default:
      goto ret0;
   }
 L5283: ATTRIBUTE_UNUSED_LABEL
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L2913;
    }
  goto ret0;

 L2936: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (signed_inequality_operator (x4, BImode))
    {
      operands[3] = x4;
      goto L2937;
    }
  goto ret0;

 L2937: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  switch (GET_MODE (x5))
    {
    case SImode:
      goto L5287;
    case DImode:
      goto L5288;
    default:
      break;
    }
  goto ret0;

 L5287: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L2938;
    }
  goto ret0;

 L2938: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0LL)
    goto L2939;
  goto ret0;

 L2939: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 484;
    }
  goto ret0;

 L5288: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L2984;
    }
  goto ret0;

 L2984: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0LL)
    goto L2985;
  goto ret0;

 L2985: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 488;
    }
  goto ret0;

 L3020: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3021;
  goto ret0;

 L3021: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3022;
    }
  goto ret0;

 L3022: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L3023;
  goto ret0;

 L3023: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3024;
    }
  goto ret0;

 L3024: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3025;
  goto ret0;

 L3025: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 491;
    }
  goto ret0;

 L3034: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3035;
  goto ret0;

 L3035: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3036;
    }
  goto ret0;

 L3036: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L3037;
  goto ret0;

 L3037: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3038;
    }
  goto ret0;

 L3038: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3039;
  goto ret0;

 L3039: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 492;
    }
  goto ret0;

 L2913: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L5289;
    case DImode:
      goto L5290;
    default:
      break;
    }
  goto ret0;

 L5289: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L2914;
    }
  goto ret0;

 L2914: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L2915;
  goto ret0;

 L2915: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 482;
    }
  goto ret0;

 L5290: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2960;
    }
  goto ret0;

 L2960: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L2961;
  goto ret0;

 L2961: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 486;
    }
  goto ret0;

 L3058: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5292;
  goto ret0;

 L5292: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case NOT:
      goto L3082;
    case NE:
      goto L3166;
    case EQ:
      goto L3180;
    case GE:
    case GT:
    case LE:
    case LT:
      goto L5291;
    default:
      goto ret0;
   }
 L5291: ATTRIBUTE_UNUSED_LABEL
  if (signed_inequality_operator (x3, BImode))
    {
      operands[3] = x3;
      goto L3059;
    }
  goto ret0;

 L3082: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (signed_inequality_operator (x4, BImode))
    {
      operands[3] = x4;
      goto L3083;
    }
  goto ret0;

 L3083: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  switch (GET_MODE (x5))
    {
    case SImode:
      goto L5295;
    case DImode:
      goto L5296;
    default:
      break;
    }
  goto ret0;

 L5295: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L3084;
    }
  goto ret0;

 L3084: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0LL)
    goto L3085;
  goto ret0;

 L3085: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 496;
    }
  goto ret0;

 L5296: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3130;
    }
  goto ret0;

 L3130: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 0LL)
    goto L3131;
  goto ret0;

 L3131: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 500;
    }
  goto ret0;

 L3166: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3167;
  goto ret0;

 L3167: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3168;
    }
  goto ret0;

 L3168: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L3169;
  goto ret0;

 L3169: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3170;
    }
  goto ret0;

 L3170: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3171;
  goto ret0;

 L3171: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 503;
    }
  goto ret0;

 L3180: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == ZERO_EXTRACT)
    goto L3181;
  goto ret0;

 L3181: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3182;
    }
  goto ret0;

 L3182: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT
      && XWINT (x5, 0) == 1LL)
    goto L3183;
  goto ret0;

 L3183: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 2);
  if (const_int_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3184;
    }
  goto ret0;

 L3184: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3185;
  goto ret0;

 L3185: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[3] = x3;
      return 504;
    }
  goto ret0;

 L3059: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  switch (GET_MODE (x4))
    {
    case SImode:
      goto L5297;
    case DImode:
      goto L5298;
    default:
      break;
    }
  goto ret0;

 L5297: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3060;
    }
  goto ret0;

 L3060: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3061;
  goto ret0;

 L3061: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 494;
    }
  goto ret0;

 L5298: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3106;
    }
  goto ret0;

 L3106: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L3107;
  goto ret0;

 L3107: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 498;
    }
  goto ret0;

 L4264: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L4265;
    }
  if (gr_reg_or_0_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4283;
    }
  goto L5282;

 L4265: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, SImode))
    {
      operands[3] = x3;
      return 608;
    }
  goto L5282;

 L4283: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_operand (x3, DImode))
    {
      operands[3] = x3;
      return 610;
    }
  goto L5282;

 L4273: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L5299;
    case DImode:
      goto L5300;
    default:
      break;
    }
  goto L5282;

 L5299: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L4274;
    }
  goto L5282;

 L4274: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_adjusted_operand (x3, SImode))
    {
      operands[3] = x3;
      return 609;
    }
  goto L5282;

 L5300: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4292;
    }
  goto L5282;

 L4292: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_8bit_adjusted_operand (x3, DImode))
    {
      operands[3] = x3;
      return 611;
    }
  goto L5282;

 L4300: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SFmode:
      goto L5301;
    case DFmode:
      goto L5302;
    case TFmode:
      goto L5303;
    default:
      break;
    }
  goto ret0;

 L5301: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L4301;
    }
  goto ret0;

 L4301: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 612;
    }
  goto ret0;

 L5302: ATTRIBUTE_UNUSED_LABEL
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L4310;
    }
  goto ret0;

 L4310: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 613;
    }
  goto ret0;

 L5303: ATTRIBUTE_UNUSED_LABEL
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L4319;
    }
  goto ret0;

 L4319: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 614;
    }
  goto ret0;

 L5269: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3232;
    }
 L5270: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3271;
    }
  goto ret0;

 L3232: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L3233;
  x2 = XEXP (x1, 0);
  goto L5270;

 L3233: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L3234;
  x2 = XEXP (x1, 0);
  goto L5270;

 L3234: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3235;
    }
  x2 = XEXP (x1, 0);
  goto L5270;

 L3235: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3236;
    }
  x2 = XEXP (x1, 0);
  goto L5270;

 L3236: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      return 509;
    }
  x2 = XEXP (x1, 0);
  goto L5270;

 L3271: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == PLUS)
    goto L3272;
  goto ret0;

 L3272: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L3273;
  goto ret0;

 L3273: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (grfr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3274;
    }
  goto ret0;

 L3274: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (grfr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3275;
    }
  goto ret0;

 L3275: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (grfr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      return 513;
    }
  goto ret0;

 L5271: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L3494;
    }
  goto ret0;

 L3494: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5304;
  goto ret0;

 L5304: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3495;
    case MINUS:
      goto L3506;
    case FLOAT_TRUNCATE:
      goto L3650;
    default:
     break;
   }
  goto ret0;

 L3495: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode)
    goto L5307;
  goto ret0;

 L5307: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L3496;
    case NEG:
      goto L3528;
    default:
     break;
   }
  goto ret0;

 L3496: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L3497;
    }
  goto ret0;

 L3497: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      goto L3498;
    }
  goto ret0;

 L3498: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 535;
    }
  goto ret0;

 L3528: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SFmode
      && GET_CODE (x4) == MULT)
    goto L3529;
  goto ret0;

 L3529: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, SFmode))
    {
      operands[1] = x5;
      goto L3530;
    }
  goto ret0;

 L3530: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, SFmode))
    {
      operands[2] = x5;
      goto L3531;
    }
  goto ret0;

 L3531: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 538;
    }
  goto ret0;

 L3506: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == MULT)
    goto L3507;
  goto ret0;

 L3507: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      goto L3508;
    }
  goto ret0;

 L3508: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, SFmode))
    {
      operands[2] = x4;
      goto L3509;
    }
  goto ret0;

 L3509: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, SFmode))
    {
      operands[3] = x3;
      return 536;
    }
  goto ret0;

 L3650: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DFmode:
      goto L5309;
    case TFmode:
      goto L5311;
    default:
      break;
    }
  goto ret0;

 L5309: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3651;
    case MINUS:
      goto L3674;
    default:
     break;
   }
  goto ret0;

 L3651: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode)
    goto L5313;
  goto ret0;

 L5313: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L3652;
    case NEG:
      goto L3735;
    default:
     break;
   }
  goto ret0;

 L3652: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3653;
    }
  goto ret0;

 L3653: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L3654;
    }
  goto ret0;

 L3654: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      return 551;
    }
  goto ret0;

 L3735: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DFmode
      && GET_CODE (x5) == MULT)
    goto L3736;
  goto ret0;

 L3736: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DFmode))
    {
      operands[1] = x6;
      goto L3737;
    }
  goto ret0;

 L3737: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (fr_register_operand (x6, DFmode))
    {
      operands[2] = x6;
      goto L3738;
    }
  goto ret0;

 L3738: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      return 558;
    }
  goto ret0;

 L3674: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L3675;
  goto ret0;

 L3675: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3676;
    }
  goto ret0;

 L3676: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L3677;
    }
  goto ret0;

 L3677: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      return 553;
    }
  goto ret0;

 L5311: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3926;
    case MINUS:
      goto L3990;
    default:
     break;
   }
  goto ret0;

 L3926: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5315;
  goto ret0;

 L5315: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L3927;
    case NEG:
      goto L4059;
    default:
     break;
   }
  goto ret0;

 L3927: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3928;
    }
  goto ret0;

 L3928: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3929;
    }
  goto ret0;

 L3929: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      return 577;
    }
  goto ret0;

 L4059: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L4060;
  goto ret0;

 L4060: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L4061;
    }
  goto ret0;

 L4061: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L4062;
    }
  goto ret0;

 L4062: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      return 588;
    }
  goto ret0;

 L3990: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L3991;
  goto ret0;

 L3991: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3992;
    }
  goto ret0;

 L3992: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3993;
    }
  goto ret0;

 L3993: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      return 582;
    }
  goto ret0;

 L5272: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L3638;
    }
  goto ret0;

 L3638: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5317;
  goto ret0;

 L5317: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3639;
    case MINUS:
      goto L3662;
    case FLOAT_TRUNCATE:
      goto L3937;
    default:
     break;
   }
  goto ret0;

 L3639: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L5320;
  goto ret0;

 L5320: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L3640;
    case NEG:
      goto L3707;
    default:
     break;
   }
  goto ret0;

 L3640: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3641;
    }
  goto ret0;

 L3641: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L3642;
    }
  goto ret0;

 L3642: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 550;
    }
  goto ret0;

 L3707: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == MULT)
    goto L3708;
  goto ret0;

 L3708: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (fr_register_operand (x5, DFmode))
    {
      operands[1] = x5;
      goto L3709;
    }
  goto ret0;

 L3709: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (fr_register_operand (x5, DFmode))
    {
      operands[2] = x5;
      goto L3710;
    }
  goto ret0;

 L3710: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 556;
    }
  goto ret0;

 L3662: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == MULT)
    goto L3663;
  goto ret0;

 L3663: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      goto L3664;
    }
  goto ret0;

 L3664: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, DFmode))
    {
      operands[2] = x4;
      goto L3665;
    }
  goto ret0;

 L3665: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_reg_or_fp01_operand (x3, DFmode))
    {
      operands[3] = x3;
      return 552;
    }
  goto ret0;

 L3937: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5322;
  goto ret0;

 L5322: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3938;
    case MINUS:
      goto L4002;
    default:
     break;
   }
  goto ret0;

 L3938: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5324;
  goto ret0;

 L5324: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L3939;
    case NEG:
      goto L4072;
    default:
     break;
   }
  goto ret0;

 L3939: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3940;
    }
  goto ret0;

 L3940: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3941;
    }
  goto ret0;

 L3941: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      return 578;
    }
  goto ret0;

 L4072: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L4073;
  goto ret0;

 L4073: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L4074;
    }
  goto ret0;

 L4074: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L4075;
    }
  goto ret0;

 L4075: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      return 589;
    }
  goto ret0;

 L4002: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4003;
  goto ret0;

 L4003: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4004;
    }
  goto ret0;

 L4004: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4005;
    }
  goto ret0;

 L4005: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      return 583;
    }
  goto ret0;

 L5273: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L3913;
    }
  goto ret0;

 L3913: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5326;
  goto ret0;

 L5326: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3914;
    case MINUS:
      goto L3978;
    default:
     break;
   }
  goto ret0;

 L3914: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5328;
  goto ret0;

 L5328: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case MULT:
      goto L3915;
    case NEG:
      goto L4046;
    default:
     break;
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
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 576;
    }
  goto ret0;

 L4046: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L4047;
  goto ret0;

 L4047: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L4048;
    }
  goto ret0;

 L4048: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L4049;
    }
  goto ret0;

 L4049: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 587;
    }
  goto ret0;

 L3978: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L3979;
  goto ret0;

 L3979: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3980;
    }
  goto ret0;

 L3980: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3981;
    }
  goto ret0;

 L3981: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[3] = x3;
      return 581;
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
      goto L2899;
    case PARALLEL:
      goto L5362;
    default:
     break;
   }
  goto ret0;

 L2899: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case BImode:
      goto L5364;
    case DImode:
      goto L5365;
    default:
      break;
    }
  goto ret0;

 L5364: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L2900;
    }
  goto ret0;

 L2900: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode)
    goto L5366;
  goto ret0;

 L5366: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L2901;
    case IOR:
      goto L3047;
    default:
     break;
   }
  goto ret0;

 L2901: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5369;
  goto ret0;

 L5369: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == NOT)
    goto L2924;
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L2948;
    }
  goto ret0;

 L2924: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (predicate_operator (x4, BImode))
    {
      operands[4] = x4;
      goto L2971;
    }
  goto ret0;

 L2971: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L2972;
    }
  if (gr_reg_or_0_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L2926;
    }
  goto ret0;

 L2972: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, DImode))
    {
      operands[3] = x5;
      goto L2973;
    }
  goto ret0;

 L2973: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 487;
    }
  goto ret0;

 L2926: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, SImode))
    {
      operands[3] = x5;
      goto L2927;
    }
  goto ret0;

 L2927: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 483;
    }
  goto ret0;

 L2948: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2949;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L2903;
    }
  goto ret0;

 L2949: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L2950;
    }
  goto ret0;

 L2950: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 485;
    }
  goto ret0;

 L2903: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L2904;
    }
  goto ret0;

 L2904: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 481;
    }
  goto ret0;

 L3047: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == BImode)
    goto L5371;
  goto ret0;

 L5371: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == NOT)
    goto L3070;
  if (predicate_operator (x3, BImode))
    {
      operands[4] = x3;
      goto L3094;
    }
  goto ret0;

 L3070: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (predicate_operator (x4, BImode))
    {
      operands[4] = x4;
      goto L3117;
    }
  goto ret0;

 L3117: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3118;
    }
  if (gr_reg_or_0_operand (x5, SImode))
    {
      operands[2] = x5;
      goto L3072;
    }
  goto ret0;

 L3118: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, DImode))
    {
      operands[3] = x5;
      goto L3119;
    }
  goto ret0;

 L3119: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 499;
    }
  goto ret0;

 L3072: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (gr_reg_or_8bit_operand (x5, SImode))
    {
      operands[3] = x5;
      goto L3073;
    }
  goto ret0;

 L3073: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 495;
    }
  goto ret0;

 L3094: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L3095;
    }
  if (gr_reg_or_0_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L3049;
    }
  goto ret0;

 L3095: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L3096;
    }
  goto ret0;

 L3096: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 497;
    }
  goto ret0;

 L3049: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_8bit_operand (x4, SImode))
    {
      operands[3] = x4;
      goto L3050;
    }
  goto ret0;

 L3050: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      return 493;
    }
  goto ret0;

 L5365: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4160;
    }
  goto ret0;

 L4160: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L4161;
  goto ret0;

 L4161: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L4162;
  goto ret0;

 L4162: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L4163;
  goto ret0;

 L4163: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (gr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L4164;
    }
  goto ret0;

 L4164: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (shladd_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L4165;
    }
  goto ret0;

 L4165: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (nonmemory_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L4166;
    }
  goto ret0;

 L4166: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[4] = x3;
      goto L4167;
    }
  goto ret0;

 L4167: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 597;
    }
  goto ret0;

 L5362: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 2:
      goto L3348;
    case 4:
      goto L4414;
    default:
      break;
    }
  goto ret0;

 L3348: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L3349;
  goto ret0;

 L3349: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5372;
    case DFmode:
      goto L5373;
    case TFmode:
      goto L5374;
    default:
      break;
    }
  goto ret0;

 L5372: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L3350;
    }
  goto ret0;

 L3350: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L3351;
  goto ret0;

 L3351: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L3352;
  goto ret0;

 L3352: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (grfr_register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L3353;
    }
  goto ret0;

 L3353: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (grfr_register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L3354;
    }
  goto ret0;

 L3354: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (grfr_register_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L3355;
    }
  goto ret0;

 L3355: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L3356;
  goto ret0;

 L3356: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[4] = x3;
      return 521;
    }
  goto ret0;

 L5373: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[0] = x3;
      goto L3718;
    }
  goto ret0;

 L3718: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DFmode)
    goto L5375;
  goto ret0;

 L5375: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L3719;
    case FLOAT_TRUNCATE:
      goto L3964;
    default:
     break;
   }
  goto ret0;

 L3719: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DFmode
      && GET_CODE (x4) == NEG)
    goto L3720;
  goto ret0;

 L3720: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DFmode
      && GET_CODE (x5) == MULT)
    goto L3721;
  goto ret0;

 L3721: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (fr_register_operand (x6, DFmode))
    {
      operands[1] = x6;
      goto L3722;
    }
  goto ret0;

 L3722: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (fr_register_operand (x6, DFmode))
    {
      operands[2] = x6;
      goto L3723;
    }
  goto ret0;

 L3723: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_reg_or_fp01_operand (x4, DFmode))
    {
      operands[3] = x4;
      goto L3724;
    }
  goto ret0;

 L3724: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3725;
  goto ret0;

 L3725: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      return 557;
    }
  goto ret0;

 L3964: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == PLUS)
    goto L3965;
  goto ret0;

 L3965: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode)
    goto L5377;
  goto ret0;

 L5377: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x5))
    {
    case MULT:
      goto L3966;
    case NEG:
      goto L4101;
    default:
     break;
   }
  goto ret0;

 L3966: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L3967;
    }
  goto ret0;

 L3967: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L3968;
    }
  goto ret0;

 L3968: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[3] = x5;
      goto L3969;
    }
  goto ret0;

 L3969: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3970;
  goto ret0;

 L3970: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      return 580;
    }
  goto ret0;

 L4101: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (GET_MODE (x6) == TFmode
      && GET_CODE (x6) == MULT)
    goto L4102;
  goto ret0;

 L4102: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 0);
  if (tfreg_or_fp01_operand (x7, TFmode))
    {
      operands[1] = x7;
      goto L4103;
    }
  goto ret0;

 L4103: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 1);
  if (tfreg_or_fp01_operand (x7, TFmode))
    {
      operands[2] = x7;
      goto L4104;
    }
  goto ret0;

 L4104: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[3] = x5;
      goto L4105;
    }
  goto ret0;

 L4105: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4106;
  goto ret0;

 L4106: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      return 591;
    }
  goto ret0;

 L5374: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[0] = x3;
      goto L3949;
    }
  goto ret0;

 L3949: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == PLUS)
    goto L3950;
  goto ret0;

 L3950: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode)
    goto L5379;
  goto ret0;

 L5379: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x4))
    {
    case MULT:
      goto L3951;
    case NEG:
      goto L4085;
    default:
     break;
   }
  goto ret0;

 L3951: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3952;
    }
  goto ret0;

 L3952: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3953;
    }
  goto ret0;

 L3953: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L3954;
    }
  goto ret0;

 L3954: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3955;
  goto ret0;

 L3955: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      return 579;
    }
  goto ret0;

 L4085: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == TFmode
      && GET_CODE (x5) == MULT)
    goto L4086;
  goto ret0;

 L4086: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[1] = x6;
      goto L4087;
    }
  goto ret0;

 L4087: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (tfreg_or_fp01_operand (x6, TFmode))
    {
      operands[2] = x6;
      goto L4088;
    }
  goto ret0;

 L4088: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[3] = x4;
      goto L4089;
    }
  goto ret0;

 L4089: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4090;
  goto ret0;

 L4090: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[4] = x3;
      return 590;
    }
  goto ret0;

 L4414: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4415;
  goto ret0;

 L4415: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  goto L4416;

 L4416: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CALL)
    goto L4417;
  goto ret0;

 L4417: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MEM)
    goto L4418;
  goto ret0;

 L4418: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (call_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L4419;
    }
  goto ret0;

 L4419: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 1LL)
    goto L4420;
  goto ret0;

 L4420: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4421;
  goto ret0;

 L4421: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4422;
    }
  goto ret0;

 L4422: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == CLOBBER)
    goto L4423;
  goto ret0;

 L4423: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L4424;
    }
  goto ret0;

 L4424: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 3);
  if (GET_CODE (x2) == CLOBBER)
    goto L4425;
  goto ret0;

 L4425: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[4] = x3;
      return 623;
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
      goto L2317;
    }
 L2339: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[3] = x1;
      goto L2340;
    }
 L2755: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[4] = x1;
      goto L2756;
    }
 L2823: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[2] = x1;
      goto L2824;
    }
 L2895: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[5] = x1;
      goto L2896;
    }
 L3358: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[6] = x1;
      goto L3359;
    }
 L4374: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[3] = x1;
      goto L4375;
    }
 L4387: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[1] = x1;
      goto L4388;
    }
 L4448: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L4449;
    }
 L4468: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[2] = x1;
      goto L4469;
    }
 L4479: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[4] = x1;
      goto L4480;
    }
 L4629: ATTRIBUTE_UNUSED_LABEL
  if (predicate_operator (x1, VOIDmode))
    {
      operands[3] = x1;
      goto L4630;
    }
  goto ret0;

 L2317: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      goto L2318;
    }
  goto L2339;

 L2318: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L2319;
  goto L2339;

 L2319: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == SET)
    goto L2320;
  x1 = XEXP (x0, 0);
  goto L2339;

 L2320: ATTRIBUTE_UNUSED_LABEL
  tem = recog_7 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L2339;

 L2340: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[4] = x2;
      goto L2341;
    }
  goto L2755;

 L2341: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L2342;
  goto L2755;

 L2342: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5162;
    case SET:
      goto L2410;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2755;

 L5162: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 3:
      goto L2343;
    case 2:
      goto L2689;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2343: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L2344;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2344: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case SImode:
      goto L5164;
    case DImode:
      goto L5165;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L5164: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L2345;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2345: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (symbolic_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L2346;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2346: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2347;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2347: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2348;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2348: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == USE)
    goto L2349;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2349: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    {
      return 421;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L5165: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2365;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2365: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (symbolic_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2366;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2366: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2367;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2367: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2368;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2368: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == USE)
    goto L2369;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2369: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == REG
      && XINT (x3, 0) == 1)
    {
      return 423;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2689: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L2690;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2690: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5167;
    case BImode:
      goto L5168;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L5167: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ZERO_EXTRACT)
    goto L2804;
  if (fr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2691;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2804: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L2805;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2805: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 32LL)
    goto L2806;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2806: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 2);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L2807;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2807: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L2808;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2808: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2809;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2809: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 472;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2691: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5169;
  x1 = XEXP (x0, 0);
  goto L2755;

 L5169: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case FIX:
      goto L2692;
    case UNSIGNED_FIX:
      goto L2751;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2692: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2693;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2693: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L2694;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2694: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[2] = x3;
      return 460;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2751: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2752;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2752: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L2753;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2753: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[2] = x3;
      return 467;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L5168: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, BImode))
    {
      operands[0] = x3;
      goto L2890;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2890: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == BImode
      && GET_CODE (x3) == NOT)
    goto L2891;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2891: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2892;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2892: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L2893;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2893: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, BImode))
    {
      operands[2] = x3;
      return 480;
    }
  x1 = XEXP (x0, 0);
  goto L2755;

 L2410: ATTRIBUTE_UNUSED_LABEL
  tem = recog_8 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L2755;

 L2756: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2757;
    }
  goto L2823;

 L2757: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L2758;
  goto L2823;

 L2758: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L2759;
    case PARALLEL:
      goto L5264;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2823;

 L2759: ATTRIBUTE_UNUSED_LABEL
  tem = recog_9 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L2823;

 L5264: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 2:
      goto L3831;
    case 4:
      goto L4399;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3831: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L3832;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3832: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case TFmode:
      goto L5330;
    case SFmode:
      goto L5331;
    case DFmode:
      goto L5332;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L5330: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, TFmode))
    {
      operands[0] = x3;
      goto L3833;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3833: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == TFmode
      && GET_CODE (x3) == MULT)
    goto L3834;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3834: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L3835;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3835: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L3836;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3836: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3837;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3837: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[3] = x3;
      return 568;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L5331: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, SFmode))
    {
      operands[0] = x3;
      goto L3845;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3845: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SFmode
      && GET_CODE (x3) == FLOAT_TRUNCATE)
    goto L3846;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3846: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L3847;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3847: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3848;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3848: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3849;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3849: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3850;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3850: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[3] = x3;
      return 569;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L5332: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x3, DFmode))
    {
      operands[0] = x3;
      goto L3858;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3858: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DFmode
      && GET_CODE (x3) == FLOAT_TRUNCATE)
    goto L3859;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3859: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TFmode
      && GET_CODE (x4) == MULT)
    goto L3860;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3860: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[1] = x5;
      goto L3861;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3861: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (tfreg_or_fp01_operand (x5, TFmode))
    {
      operands[2] = x5;
      goto L3862;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L3862: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L3863;
  x1 = XEXP (x0, 0);
  goto L2823;

 L3863: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (const_int_operand (x3, SImode))
    {
      operands[3] = x3;
      return 570;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L4399: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CALL)
    goto L4400;
  x1 = XEXP (x0, 0);
  goto L2823;

 L4400: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == MEM)
    goto L4401;
  x1 = XEXP (x0, 0);
  goto L2823;

 L4401: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, VOIDmode))
    {
      operands[0] = x4;
      goto L4402;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L4402: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    goto L4403;
  x1 = XEXP (x0, 0);
  goto L2823;

 L4403: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4404;
  x1 = XEXP (x0, 0);
  goto L2823;

 L4404: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4405;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L4405: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == CLOBBER)
    goto L4406;
  x1 = XEXP (x0, 0);
  goto L2823;

 L4406: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L4407;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L4407: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 3);
  if (GET_CODE (x2) == CLOBBER)
    goto L4408;
  x1 = XEXP (x0, 0);
  goto L2823;

 L4408: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[3] = x3;
      return 622;
    }
  x1 = XEXP (x0, 0);
  goto L2823;

 L2824: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      goto L2825;
    }
  goto L2895;

 L2825: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L2826;
  goto L2895;

 L2826: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L2827;
    case PARALLEL:
      goto L5333;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2895;

 L2827: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case DImode:
      goto L5334;
    case SImode:
      goto L5335;
    case SFmode:
      goto L5337;
    case DFmode:
      goto L5338;
    case TFmode:
      goto L5339;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L5334: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L2828;
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L3310;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L2828: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2829;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L2829: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL)
    goto L2830;
  x1 = XEXP (x0, 0);
  goto L2895;

 L2830: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL)
    goto L2831;
  x1 = XEXP (x0, 0);
  goto L2895;

 L2831: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_0_operand (x2, DImode))
    {
      operands[1] = x2;
      return 474;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3310: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode)
    goto L5340;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5340: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3311;
    case NEG:
      goto L3409;
    case UNSPEC:
      goto L5346;
    case NOT:
      goto L4256;
    case NE:
      goto L4351;
    case EQ:
      goto L4360;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3311: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L3312;
  x1 = XEXP (x0, 0);
  goto L2895;

 L3312: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L3313;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3313: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 2LL)
    goto L3314;
  x1 = XEXP (x0, 0);
  goto L2895;

 L3314: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    {
      return 517;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3409: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 525;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L5346: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 16)
    goto L3417;
  x1 = XEXP (x0, 0);
  goto L2895;

 L3417: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 526;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L4256: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 607;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L4351: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L4352;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L4352: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    {
      return 617;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L4360: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L4361;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L4361: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    {
      return 618;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L5335: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L3221;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3221: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode)
    goto L5347;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5347: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case PLUS:
      goto L3222;
    case NEG:
      goto L3283;
    case NOT:
      goto L4211;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3222: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == MULT)
    goto L3223;
  x1 = XEXP (x0, 0);
  goto L2895;

 L3223: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L3224;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3224: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 2LL)
    goto L3225;
  x1 = XEXP (x0, 0);
  goto L2895;

 L3225: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    {
      return 508;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3283: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 514;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L4211: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 602;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L5337: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L3451;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3451: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode)
    goto L5350;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5350: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L3452;
    case NEG:
      goto L3460;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3452: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 530;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3460: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode)
    goto L5353;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5353: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ABS)
    goto L3469;
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      return 531;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3469: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, SFmode))
    {
      operands[1] = x4;
      return 532;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L5338: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L3595;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3595: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode)
    goto L5354;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5354: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L3596;
    case NEG:
      goto L3604;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3596: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 545;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3604: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode)
    goto L5357;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5357: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ABS)
    goto L3613;
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      return 546;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3613: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, DFmode))
    {
      operands[1] = x4;
      return 547;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L5339: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L3870;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3870: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5358;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5358: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case ABS:
      goto L3871;
    case NEG:
      goto L3879;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3871: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 571;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3879: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TFmode)
    goto L5361;
  x1 = XEXP (x0, 0);
  goto L2895;

 L5361: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) == ABS)
    goto L3888;
  if (tfreg_or_fp01_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 572;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L3888: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (tfreg_or_fp01_operand (x4, TFmode))
    {
      operands[1] = x4;
      return 573;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L5333: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4367;
  x1 = XEXP (x0, 0);
  goto L2895;

 L4367: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CALL)
    goto L4368;
  x1 = XEXP (x0, 0);
  goto L2895;

 L4368: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L4369;
  x1 = XEXP (x0, 0);
  goto L2895;

 L4369: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L4370;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L4370: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L4371;
  x1 = XEXP (x0, 0);
  goto L2895;

 L4371: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4372;
  x1 = XEXP (x0, 0);
  goto L2895;

 L4372: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      return 619;
    }
  x1 = XEXP (x0, 0);
  goto L2895;

 L2896: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2897;
    }
  goto L3358;

 L2897: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L2898;
  goto L3358;

 L2898: ATTRIBUTE_UNUSED_LABEL
  tem = recog_10 (x0, insn, pnum_clobbers);
  if (tem >= 0)
    return tem;
  x1 = XEXP (x0, 0);
  goto L3358;

 L3359: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[7] = x2;
      goto L3360;
    }
  goto L4374;

 L3360: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L3361;
  goto L4374;

 L3361: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == PARALLEL
      && XVECLEN (x1, 0) == 2)
    goto L3362;
  x1 = XEXP (x0, 0);
  goto L4374;

 L3362: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L3363;
  x1 = XEXP (x0, 0);
  goto L4374;

 L3363: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L3364;
    }
  x1 = XEXP (x0, 0);
  goto L4374;

 L3364: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L3365;
  x1 = XEXP (x0, 0);
  goto L4374;

 L3365: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == PLUS)
    goto L3366;
  x1 = XEXP (x0, 0);
  goto L4374;

 L3366: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DImode
      && GET_CODE (x5) == MULT)
    goto L3367;
  x1 = XEXP (x0, 0);
  goto L4374;

 L3367: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, DImode))
    {
      operands[1] = x6;
      goto L3368;
    }
  x1 = XEXP (x0, 0);
  goto L4374;

 L3368: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (register_operand (x6, DImode))
    {
      operands[2] = x6;
      goto L3369;
    }
  x1 = XEXP (x0, 0);
  goto L4374;

 L3369: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (register_operand (x5, DImode))
    {
      operands[3] = x5;
      goto L3370;
    }
  x1 = XEXP (x0, 0);
  goto L4374;

 L3370: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (nonmemory_operand (x4, DImode))
    {
      operands[4] = x4;
      goto L3371;
    }
  x1 = XEXP (x0, 0);
  goto L4374;

 L3371: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L3372;
  x1 = XEXP (x0, 0);
  goto L4374;

 L3372: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[5] = x3;
      goto L3373;
    }
  x1 = XEXP (x0, 0);
  goto L4374;

 L3373: ATTRIBUTE_UNUSED_LABEL
  if ((reload_in_progress))
    {
      return 522;
    }
  x1 = XEXP (x0, 0);
  goto L4374;

 L4375: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[4] = x2;
      goto L4376;
    }
  goto L4387;

 L4376: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L4377;
  goto L4387;

 L4377: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5381;
    case PREFETCH:
      goto L4598;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4387;

 L5381: ATTRIBUTE_UNUSED_LABEL
  switch (XVECLEN (x1, 0))
    {
    case 2:
      goto L4378;
    case 3:
      goto L4431;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4378: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4379;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4379: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  goto L4380;
 L4621: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L4622;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4380: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CALL)
    goto L4381;
  x3 = XEXP (x2, 0);
  goto L4621;

 L4381: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MEM)
    goto L4382;
  x3 = XEXP (x2, 0);
  goto L4621;

 L4382: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (call_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L4383;
    }
  x3 = XEXP (x2, 0);
  goto L4621;

 L4383: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L4384;
  x3 = XEXP (x2, 0);
  goto L4621;

 L4384: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4385;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L4621;

 L4385: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 620;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L4621;

 L4622: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    goto L4623;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4623: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4624;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4624: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4625;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4625: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 20)
    goto L4626;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4626: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4627;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4627: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (fetchadd_operand (x4, SImode))
    {
      operands[2] = x4;
      return 648;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4431: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CALL)
    goto L4432;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4432: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L4433;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4433: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[0] = x4;
      goto L4434;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4434: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    goto L4435;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4435: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4436;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4436: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4437;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4437: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) == CLOBBER)
    goto L4438;
  x1 = XEXP (x0, 0);
  goto L4387;

 L4438: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (scratch_operand (x3, DImode))
    {
      operands[2] = x3;
      return 624;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4598: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (address_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4599;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4599: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L4600;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4600: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (const_int_operand (x2, DImode))
    {
      operands[2] = x2;
      return 645;
    }
  x1 = XEXP (x0, 0);
  goto L4387;

 L4388: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L4389;
    }
  goto L4448;

 L4389: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L4390;
  goto L4448;

 L4390: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case CALL:
      goto L4391;
    case PARALLEL:
      goto L5383;
    case SET:
      goto L4547;
    case TRAP_IF:
      goto L4585;
    case UNSPEC_VOLATILE:
      goto L5384;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4448;

 L4391: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L4392;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4392: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4393;
    }
  x1 = XEXP (x0, 0);
  goto L4448;

 L4393: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return 621;
    }
  x1 = XEXP (x0, 0);
  goto L4448;

 L5383: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4444;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4444: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == RETURN)
    goto L4445;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4445: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4446;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4446: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      return 625;
    }
  x1 = XEXP (x0, 0);
  goto L4448;

 L4547: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4548;
    }
  if (GET_CODE (x2) == PC)
    goto L4466;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4548: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 21)
    goto L4549;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4549: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    {
      return 636;
    }
  x1 = XEXP (x0, 0);
  goto L4448;

 L4466: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      return 628;
    }
  if (GET_CODE (x2) == LABEL_REF)
    goto L4459;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4459: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  return 627;

 L4585: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    goto L4586;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4586: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (const_int_operand (x2, VOIDmode))
    {
      operands[0] = x2;
      return 643;
    }
  x1 = XEXP (x0, 0);
  goto L4448;

 L5384: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 7)
    goto L4606;
  x1 = XEXP (x0, 0);
  goto L4448;

 L4606: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  operands[0] = x2;
  return 646;

 L4449: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L4450;
    }
  goto L4468;

 L4450: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L4451;
  goto L4468;

 L4451: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case RETURN:
      goto L5385;
    case CONST_INT:
      goto L5386;
    case UNSPEC_VOLATILE:
      goto L5392;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4468;

 L5385: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_direct_return ()))
    {
      return 626;
    }
  x1 = XEXP (x0, 0);
  goto L4468;

 L5386: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x1, 0) == XWINT (x1, 0))
    switch ((int) XWINT (x1, 0))
      {
      case 0LL:
        goto L5393;
      case 1LL:
        goto L5394;
      case 2LL:
        goto L5395;
      case 3LL:
        goto L5396;
      case 4LL:
        goto L5397;
      case 5LL:
        goto L5398;
      default:
        break;
      }
  x1 = XEXP (x0, 0);
  goto L4468;

 L5393: ATTRIBUTE_UNUSED_LABEL
  return 637;

 L5394: ATTRIBUTE_UNUSED_LABEL
  return 638;

 L5395: ATTRIBUTE_UNUSED_LABEL
  return 639;

 L5396: ATTRIBUTE_UNUSED_LABEL
  return 640;

 L5397: ATTRIBUTE_UNUSED_LABEL
  return 641;

 L5398: ATTRIBUTE_UNUSED_LABEL
  return 642;

 L5392: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 1
      && XINT (x1, 1) == 3)
    goto L4592;
  x1 = XEXP (x0, 0);
  goto L4468;

 L4592: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return 644;
    }
  x1 = XEXP (x0, 0);
  goto L4468;

 L4469: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[3] = x2;
      goto L4470;
    }
  goto L4479;

 L4470: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L4471;
  goto L4479;

 L4471: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5399;
    case SET:
      goto L4531;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L4479;

 L5399: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4472;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4472: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4497;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4497: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4498;
    }
  if (GET_CODE (x3) == PC)
    goto L4474;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4498: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4499;
    }
  x1 = XEXP (x0, 0);
  goto L4479;

 L4499: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4500;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4500: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L4501;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4501: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    {
      return 631;
    }
  x1 = XEXP (x0, 0);
  goto L4479;

 L4474: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4475;
    }
  x1 = XEXP (x0, 0);
  goto L4479;

 L4475: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4476;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4476: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == LABEL_REF)
    goto L4477;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4477: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  operands[1] = x4;
  return 629;

 L4531: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5400;
 L4612: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L4613;
 L4696: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4697;
    }
  x1 = XEXP (x0, 0);
  goto L4479;

 L5400: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L4532;
    }
 L5401: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L4540;
    }
  goto L4612;

 L4532: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 12)
    goto L4533;
  x2 = XEXP (x1, 0);
  goto L5401;

 L4533: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (register_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 634;
    }
  x2 = XEXP (x1, 0);
  goto L5401;

 L4540: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 13)
    goto L4541;
  x2 = XEXP (x1, 0);
  goto L4612;

 L4541: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (memory_operand (x3, TFmode))
    {
      operands[1] = x3;
      return 635;
    }
  x2 = XEXP (x1, 0);
  goto L4612;

 L4613: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BLKmode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 18)
    goto L4614;
  x2 = XEXP (x1, 0);
  goto L4696;

 L4614: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  operands[1] = x3;
  return 647;

 L4697: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 24)
    goto L4698;
  x1 = XEXP (x0, 0);
  goto L4479;

 L4698: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (gr_register_operand (x3, SImode))
    {
      operands[1] = x3;
      return 654;
    }
  x1 = XEXP (x0, 0);
  goto L4479;

 L4480: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L4481;
    }
  goto L4629;

 L4481: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L4482;
  goto L4629;

 L4482: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == PARALLEL
      && XVECLEN (x1, 0) == 2)
    goto L4483;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4483: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4484;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4484: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5402;
    case SImode:
      goto L5404;
    default:
      break;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L5402: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4485;
    }
 L5403: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4509;
    }
 L5405: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4662;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L4485: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode)
    goto L5406;
  x3 = XEXP (x2, 0);
  goto L5403;

 L5406: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x3))
    {
    case PLUS:
      goto L4486;
    case UNSPEC:
      goto L5408;
    default:
     break;
   }
  x3 = XEXP (x2, 0);
  goto L5403;

 L4486: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4487;
    }
  x3 = XEXP (x2, 0);
  goto L5403;

 L4487: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_22bit_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4488;
    }
  x3 = XEXP (x2, 0);
  goto L5403;

 L4488: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4489;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5403;

 L4489: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L4490;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5403;

 L4490: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[3]))
    {
      return 630;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5403;

 L5408: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 11)
    goto L4522;
  x3 = XEXP (x2, 0);
  goto L5403;

 L4522: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (memory_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4523;
    }
  x3 = XEXP (x2, 0);
  goto L5403;

 L4523: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4524;
    }
  x3 = XEXP (x2, 0);
  goto L5403;

 L4524: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == USE)
    goto L4525;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5403;

 L4525: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      return 633;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5403;

 L4509: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 10)
    goto L4510;
  x3 = XEXP (x2, 0);
  goto L5405;

 L4510: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L4511;
    }
  x3 = XEXP (x2, 0);
  goto L5405;

 L4511: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (const_int_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4512;
    }
  x3 = XEXP (x2, 0);
  goto L5405;

 L4512: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == CLOBBER)
    goto L4513;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5405;

 L4513: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      return 632;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  goto L5405;

 L4662: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    goto L4663;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4663: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4664;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4664: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4665;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L4665: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 3
      && XINT (x3, 1) == 19)
    goto L4666;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4666: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4667;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4667: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (gr_register_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L4668;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L4668: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 2);
  if (ar_ccv_reg_operand (x4, DImode))
    {
      operands[3] = x4;
      return 651;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L5404: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L4648;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L4648: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, operands[1]))
    goto L4649;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4649: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4650;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4650: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4651;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L4651: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 3
      && XINT (x3, 1) == 19)
    goto L4652;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4652: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4653;
  x1 = XEXP (x0, 0);
  goto L4629;

 L4653: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (gr_register_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L4654;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L4654: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 2);
  if (ar_ccv_reg_operand (x4, VOIDmode))
    {
      operands[3] = x4;
      return 650;
    }
  x1 = XEXP (x0, 0);
  goto L4629;

 L4630: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[4] = x2;
      goto L4631;
    }
  goto ret0;

 L4631: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L4632;
  goto ret0;

 L4632: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      goto L5409;
    case SET:
      goto L4704;
    default:
     break;
   }
  goto ret0;

 L5409: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x1, 0) == 2)
    goto L4633;
  goto ret0;

 L4633: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (GET_CODE (x2) == SET)
    goto L4634;
  goto ret0;

 L4634: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    case DImode:
      goto L5410;
    case SImode:
      goto L5411;
    default:
      break;
    }
  goto ret0;

 L5410: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L4687;
    }
  goto ret0;

 L4687: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (not_postinc_memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4688;
    }
 L4635: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x3, operands[1]))
    goto L4636;
  goto ret0;

 L4688: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4689;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  goto L4635;

 L4689: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L4690;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  goto L4635;

 L4690: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, DImode))
    {
      operands[2] = x3;
      return 653;
    }
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  goto L4635;

 L4636: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4637;
  goto ret0;

 L4637: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (not_postinc_memory_operand (x3, DImode))
    {
      operands[1] = x3;
      goto L4638;
    }
  goto ret0;

 L4638: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == UNSPEC
      && XVECLEN (x3, 0) == 2
      && XINT (x3, 1) == 20)
    goto L4639;
  goto ret0;

 L4639: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 0);
  if (rtx_equal_p (x4, operands[1]))
    goto L4640;
  goto ret0;

 L4640: ATTRIBUTE_UNUSED_LABEL
  x4 = XVECEXP (x3, 0, 1);
  if (fetchadd_operand (x4, DImode))
    {
      operands[2] = x4;
      return 649;
    }
  goto ret0;

 L5411: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x3, SImode))
    {
      operands[0] = x3;
      goto L4676;
    }
  goto ret0;

 L4676: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (not_postinc_memory_operand (x3, SImode))
    {
      operands[1] = x3;
      goto L4677;
    }
  goto ret0;

 L4677: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) == SET)
    goto L4678;
  goto ret0;

 L4678: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, operands[1]))
    goto L4679;
  goto ret0;

 L4679: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      return 652;
    }
  goto ret0;

 L4704: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L4705;
    }
  goto ret0;

 L4705: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 24)
    goto L4706;
  goto ret0;

 L4706: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == PLUS)
    goto L4707;
  goto ret0;

 L4707: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SImode)
    goto L5412;
  goto ret0;

 L5412: ATTRIBUTE_UNUSED_LABEL
  if (basereg_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L4708;
    }
 L5413: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x4, SImode))
    {
      operands[1] = x4;
      goto L4719;
    }
  goto ret0;

 L4708: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (gr_reg_or_14bit_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L4709;
    }
  x4 = XEXP (x3, 0);
  goto L5413;

 L4709: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 655;
    }
  x1 = XEXP (x0, 1);
  x2 = XEXP (x1, 1);
  x3 = XVECEXP (x2, 0, 0);
  x4 = XEXP (x3, 0);
  goto L5413;

 L4719: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (basereg_operand (x4, SImode))
    {
      operands[2] = x4;
      goto L4720;
    }
  goto ret0;

 L4720: ATTRIBUTE_UNUSED_LABEL
  if ((addp4_optimize_ok (operands[1], operands[2])))
    {
      return 656;
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
    goto L1697;
  switch (GET_CODE (x0))
    {
    case SET:
      goto L1;
    case PARALLEL:
      goto L4721;
    case CALL:
      goto L1587;
    case RETURN:
      goto L4726;
    case UNSPEC_VOLATILE:
      goto L4727;
    case UNSPEC:
      goto L4728;
    case CONST_INT:
      goto L4729;
    case TRAP_IF:
      goto L1756;
    case PREFETCH:
      goto L1766;
    case COND_EXEC:
      goto L2316;
    default:
      goto ret0;
   }
 L1741: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x0))
    {
    case UNSPEC:
      goto L4728;
    case CONST_INT:
      goto L4729;
    case TRAP_IF:
      goto L1756;
    case PREFETCH:
      goto L1766;
    case COND_EXEC:
      goto L2316;
    default:
     break;
   }
  goto ret0;

 L1697: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      return 238;
    }
  goto L1741;

 L1: ATTRIBUTE_UNUSED_LABEL
  return recog_4 (x0, insn, pnum_clobbers);

 L4721: ATTRIBUTE_UNUSED_LABEL
  return recog_6 (x0, insn, pnum_clobbers);

 L1587: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (GET_MODE (x1) == DImode
      && GET_CODE (x1) == MEM)
    goto L1588;
  goto ret0;

 L1588: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (call_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1589;
    }
  goto ret0;

 L1589: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == CONST_INT)
    goto L5078;
  goto ret0;

 L5078: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x1, 0) == XWINT (x1, 0))
    switch ((int) XWINT (x1, 0))
      {
      case 0LL:
        goto L5080;
      case 1LL:
        goto L5081;
      default:
        break;
      }
  goto ret0;

 L5080: ATTRIBUTE_UNUSED_LABEL
  return 225;

 L5081: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != NULL)
    {
      *pnum_clobbers = 2;
      return 228;
    }
  goto ret0;

 L4726: ATTRIBUTE_UNUSED_LABEL
  if ((ia64_direct_return ()))
    {
      return 230;
    }
  goto ret0;

 L4727: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1)
    goto L5082;
  goto ret0;

 L5082: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x0, 1))
    {
    case 4LL:
      goto L1740;
    case 1LL:
      goto L1752;
    case 2LL:
      goto L1754;
    case 3LL:
      goto L1764;
    case 7LL:
      goto L1770;
    case 5LL:
      goto L1832;
    case 6LL:
      goto L1834;
    default:
      break;
    }
  goto ret0;

 L1740: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      return 245;
    }
  goto ret0;

 L1752: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL)
    {
      return 254;
    }
  goto ret0;

 L1754: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      return 255;
    }
  goto ret0;

 L1764: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL)
    {
      return 258;
    }
  goto ret0;

 L1770: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  operands[0] = x1;
  return 260;

 L1832: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL)
    {
      return 269;
    }
  goto ret0;

 L1834: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL)
    {
      return 270;
    }
  goto ret0;

 L4728: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1)
    goto L5089;
  goto ret0;

 L5089: ATTRIBUTE_UNUSED_LABEL
  switch (XINT (x0, 1))
    {
    case 22LL:
      goto L1742;
    case 23LL:
      goto L1750;
    default:
      break;
    }
  goto ret0;

 L1742: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 0LL)
    {
      return 246;
    }
  goto ret0;

 L1750: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      return 253;
    }
  goto ret0;

 L4729: ATTRIBUTE_UNUSED_LABEL
  if ((int) XWINT (x0, 0) == XWINT (x0, 0))
    switch ((int) XWINT (x0, 0))
      {
      case 0LL:
        goto L5091;
      case 1LL:
        goto L5092;
      case 2LL:
        goto L5093;
      case 3LL:
        goto L5094;
      case 4LL:
        goto L5095;
      case 5LL:
        goto L5096;
      default:
        break;
      }
  goto ret0;

 L5091: ATTRIBUTE_UNUSED_LABEL
  return 247;

 L5092: ATTRIBUTE_UNUSED_LABEL
  return 248;

 L5093: ATTRIBUTE_UNUSED_LABEL
  return 249;

 L5094: ATTRIBUTE_UNUSED_LABEL
  return 250;

 L5095: ATTRIBUTE_UNUSED_LABEL
  return 251;

 L5096: ATTRIBUTE_UNUSED_LABEL
  return 252;

 L1756: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (GET_CODE (x1) == CONST_INT
      && XWINT (x1, 0) == 1LL)
    goto L1757;
  if (predicate_operator (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L1760;
    }
  goto ret0;

 L1757: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      return 256;
    }
  goto ret0;

 L1760: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1761;
    }
  goto ret0;

 L1761: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    goto L1762;
  goto ret0;

 L1762: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, VOIDmode))
    {
      operands[2] = x1;
      return 257;
    }
  goto ret0;

 L1766: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (address_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1767;
    }
  goto ret0;

 L1767: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (const_int_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L1768;
    }
  goto ret0;

 L1768: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 2);
  if (const_int_operand (x1, DImode))
    {
      operands[2] = x1;
      return 259;
    }
  goto ret0;

 L2316: ATTRIBUTE_UNUSED_LABEL
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
      goto L5420;
    case DImode:
      goto L5421;
    case TImode:
      goto L5422;
    case SImode:
      goto L5423;
    default:
      break;
    }
 L2180: ATTRIBUTE_UNUSED_LABEL
  if (destination_operand (x1, VOIDmode))
    {
      operands[0] = x1;
      goto L2181;
    }
  goto ret0;

 L5420: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L1855;
    }
  goto L2180;

 L1855: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BImode)
    goto L5427;
  x1 = XEXP (x0, 0);
  goto L2180;

 L5427: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case AND:
      goto L1915;
    case IOR:
      goto L1928;
    case NE:
      goto L1957;
    case EQ:
      goto L1966;
    case SUBREG:
    case REG:
    case ADDRESSOF:
      goto L5426;
    default:
      x1 = XEXP (x0, 0);
      goto L2180;
   }
 L5426: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, BImode))
    {
      operands[1] = x1;
      goto L1856;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1915: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L5432;
  x1 = XEXP (x0, 0);
  goto L2180;

 L5432: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L1922;
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1916;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1922: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1923;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1923: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1924;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1924: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_302 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1916: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1917;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1917: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[2]) == REG && PR_REGNO_P (REGNO (operands[2]))))
    {
      return gen_split_301 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1928: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BImode)
    goto L5434;
  x1 = XEXP (x0, 0);
  goto L2180;

 L5434: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == NOT)
    goto L1935;
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L1929;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1935: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1936;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1936: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1937;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1937: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_304 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1929: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1930;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1930: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[2]) == REG && PR_REGNO_P (REGNO (operands[2]))))
    {
      return gen_split_303 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1957: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L5435;
  x1 = XEXP (x0, 0);
  goto L2180;

 L5435: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L1958;
    case IOR:
      goto L1976;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1958: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L1959;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1959: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L1960;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1960: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1961;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1961: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L1962;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1962: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return gen_split_307 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1976: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L1977;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1977: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L1978;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1978: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1979;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1979: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L1980;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1980: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return gen_split_309 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1966: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode)
    goto L5437;
  x1 = XEXP (x0, 0);
  goto L2180;

 L5437: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case AND:
      goto L1967;
    case IOR:
      goto L1985;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1967: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L1968;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1968: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L1969;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1969: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1970;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1970: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L1971;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1971: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return gen_split_308 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1985: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == NE)
    goto L1986;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1986: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[2] = x4;
      goto L1987;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1987: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L1988;
  x1 = XEXP (x0, 0);
  goto L2180;

 L1988: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L1989;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1989: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL)
    {
      return gen_split_310 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1856: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && GR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_274 (operands);
    }
 L1860: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))))
    {
      return gen_split_275 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L5421: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L1879;
    }
 L5424: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, DImode))
    {
      operands[0] = x1;
      goto L2161;
    }
  goto L2180;

 L1879: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L5440;
  x1 = XEXP (x0, 0);
  goto L5424;

 L5440: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x1) == IF_THEN_ELSE)
    goto L2191;
  if (symbolic_operand (x1, DImode))
    {
      operands[1] = x1;
      goto L1880;
    }
  x1 = XEXP (x0, 0);
  goto L5424;

 L2191: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2192;
    }
  x1 = XEXP (x0, 0);
  goto L5424;

 L2192: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2193;
    }
  x1 = XEXP (x0, 0);
  goto L5424;

 L2193: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2194;
  x1 = XEXP (x0, 0);
  goto L5424;

 L2194: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == NEG)
    goto L2195;
  x1 = XEXP (x0, 0);
  goto L5424;

 L2195: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, DImode))
    {
      operands[2] = x3;
      goto L2196;
    }
  x1 = XEXP (x0, 0);
  goto L5424;

 L2196: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2197;
    }
  x1 = XEXP (x0, 0);
  goto L5424;

 L2197: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && rtx_equal_p (operands[0], operands[3])))
    {
      return gen_split_388 (operands);
    }
 L2207: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_389 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5424;

 L1880: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && ! TARGET_NO_PIC))
    {
      return gen_split_282 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5424;

 L2161: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode)
    goto L5441;
  x1 = XEXP (x0, 0);
  goto L2180;

 L5441: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case PLUS:
      goto L2162;
    case NE:
      goto L2172;
    case EQ:
      goto L2177;
    default:
     break;
   }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2162: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L2163;
  x1 = XEXP (x0, 0);
  goto L2180;

 L2163: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MULT)
    goto L2164;
  x1 = XEXP (x0, 0);
  goto L2180;

 L2164: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (gr_register_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L2165;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2165: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (shladd_operand (x4, DImode))
    {
      operands[2] = x4;
      goto L2166;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2166: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, DImode))
    {
      operands[3] = x3;
      goto L2167;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2167: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (nonmemory_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L2168;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2168: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_351 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2172: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L2173;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2173: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL
      && (reload_completed))
    {
      return gen_split_385 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2177: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[1] = x2;
      goto L2178;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2178: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 0LL
      && (reload_completed))
    {
      return gen_split_386 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L5422: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, TImode))
    {
      operands[0] = x1;
      goto L1890;
    }
  goto L2180;

 L1890: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (nonmemory_operand (x1, TImode))
    {
      operands[1] = x1;
      goto L1891;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L1891: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_292 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L5423: ATTRIBUTE_UNUSED_LABEL
  if (gr_register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2149;
    }
 L5425: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, SImode))
    {
      operands[0] = x1;
      goto L2210;
    }
  goto L2180;

 L2149: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode)
    goto L5444;
  x1 = XEXP (x0, 0);
  goto L5425;

 L5444: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x1))
    {
    case ROTATERT:
      goto L2150;
    case ROTATE:
      goto L2156;
    case PLUS:
    case MINUS:
    case IOR:
    case XOR:
    case AND:
      goto L5446;
    default:
      x1 = XEXP (x0, 0);
      goto L5425;
   }
 L5446: ATTRIBUTE_UNUSED_LABEL
  if (condop_operator (x1, SImode))
    {
      operands[5] = x1;
      goto L2231;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2150: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2151;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2151: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_reg_or_5bit_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2152;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2152: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_348 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2156: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (gr_register_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L2157;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (shift_32bit_count_operand (x2, SImode))
    {
      operands[2] = x2;
      goto L2158;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2158: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_350 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2231: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode)
    goto L5447;
  x1 = XEXP (x0, 0);
  goto L5425;

 L5447: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == IF_THEN_ELSE)
    goto L2232;
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L2243;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2232: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L2233;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2233: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2234;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2234: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L2235;
  x1 = XEXP (x0, 0);
  goto L5425;

 L2235: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2236;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2236: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L2237;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2237: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (gr_register_operand (x2, SImode))
    {
      operands[4] = x2;
      goto L2238;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2238: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_392 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2243: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == IF_THEN_ELSE)
    goto L2244;
  x1 = XEXP (x0, 0);
  goto L5425;

 L2244: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (predicate_operator (x3, VOIDmode))
    {
      operands[6] = x3;
      goto L2245;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2245: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, BImode))
    {
      operands[1] = x4;
      goto L2246;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2246: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT
      && XWINT (x4, 0) == 0LL)
    goto L2247;
  x1 = XEXP (x0, 0);
  goto L5425;

 L2247: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_register_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2248;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2248: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (gr_register_operand (x3, SImode))
    {
      operands[3] = x3;
      goto L2249;
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2249: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_393 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L5425;

 L2210: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode
      && GET_CODE (x1) == IF_THEN_ELSE)
    goto L2211;
  x1 = XEXP (x0, 0);
  goto L2180;

 L2211: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2212;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2212: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2213;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2213: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2214;
  x1 = XEXP (x0, 0);
  goto L2180;

 L2214: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode
      && GET_CODE (x2) == NEG)
    goto L2215;
  x1 = XEXP (x0, 0);
  goto L2180;

 L2215: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (gr_reg_or_22bit_operand (x3, SImode))
    {
      operands[2] = x3;
      goto L2216;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2216: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (gr_reg_or_22bit_operand (x2, SImode))
    {
      operands[3] = x2;
      goto L2217;
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2217: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && rtx_equal_p (operands[0], operands[3])))
    {
      return gen_split_390 (operands);
    }
 L2227: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_391 (operands);
    }
  x1 = XEXP (x0, 0);
  goto L2180;

 L2181: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_CODE (x1) == IF_THEN_ELSE)
    goto L2182;
  goto ret0;

 L2182: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (predicate_operator (x2, VOIDmode))
    {
      operands[4] = x2;
      goto L2183;
    }
  goto ret0;

 L2183: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L2184;
    }
  goto ret0;

 L2184: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L2185;
  goto ret0;

 L2185: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (move_operand (x2, VOIDmode))
    {
      operands[2] = x2;
      goto L2186;
    }
  goto ret0;

 L2186: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (move_operand (x2, VOIDmode))
    {
      operands[3] = x2;
      goto L2187;
    }
  goto ret0;

 L2187: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_387 (operands);
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
      goto L1862;
    case 2:
      goto L1882;
    case 5:
      goto L2005;
    case 4:
      goto L2050;
    case 6:
      goto L2118;
    default:
      break;
    }
  goto ret0;

 L1862: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L1863;
    case CALL:
      goto L2304;
    default:
     break;
   }
  goto ret0;

 L1863: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case SImode:
      goto L5449;
    case DImode:
      goto L5450;
    default:
      break;
    }
  goto ret0;

 L5449: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      operands[0] = x2;
      goto L1864;
    }
  goto ret0;

 L1864: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (symbolic_operand (x2, SImode))
    {
      operands[1] = x2;
      goto L1865;
    }
  goto ret0;

 L1865: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1866;
  goto ret0;

 L1866: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1867;
    }
  goto ret0;

 L1867: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1868;
  goto ret0;

 L1868: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1
      && (!no_new_pseudos || reload_completed))
    {
      return gen_split_279 (operands);
    }
  goto ret0;

 L5450: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L1872;
    }
  goto ret0;

 L1872: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (symbolic_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1873;
    }
  goto ret0;

 L1873: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1874;
  goto ret0;

 L1874: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1875;
    }
  goto ret0;

 L1875: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == USE)
    goto L1876;
  goto ret0;

 L1876: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == REG
      && XINT (x2, 0) == 1
      && (!no_new_pseudos || reload_completed))
    {
      return gen_split_281 (operands);
    }
  goto ret0;

 L2304: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == MEM)
    goto L2305;
  goto ret0;

 L2305: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L2306;
    }
  goto ret0;

 L2306: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    goto L2307;
  goto ret0;

 L2307: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2308;
  goto ret0;

 L2308: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2309;
    }
  goto ret0;

 L2309: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2310;
  goto ret0;

 L2310: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2311;
    }
  goto ret0;

 L2311: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_403 (operands);
    }
  goto ret0;

 L1882: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L1883;
  goto ret0;

 L1883: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TImode:
      goto L5451;
    case DImode:
      goto L5452;
    case BImode:
      goto L5453;
    default:
      break;
    }
  goto ret0;

 L5451: ATTRIBUTE_UNUSED_LABEL
  if (nonimmediate_operand (x2, TImode))
    {
      operands[0] = x2;
      goto L1884;
    }
  goto ret0;

 L1884: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (general_operand (x2, TImode))
    {
      operands[1] = x2;
      goto L1885;
    }
  goto ret0;

 L1885: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1886;
  goto ret0;

 L1886: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1887;
    }
  goto ret0;

 L1887: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_291 (operands);
    }
  goto ret0;

 L5452: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) == ZERO_EXTRACT)
    goto L1895;
  if (register_operand (x2, DImode))
    {
      operands[0] = x2;
      goto L2023;
    }
  goto ret0;

 L1895: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      operands[0] = x3;
      goto L1896;
    }
  goto ret0;

 L1896: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 32LL)
    goto L1897;
  goto ret0;

 L1897: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 2);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 0LL)
    goto L1898;
  goto ret0;

 L1898: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L1899;
    }
  goto ret0;

 L1899: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1900;
  goto ret0;

 L1900: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L1901;
    }
  goto ret0;

 L1901: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_299 (operands);
    }
 L1911: ATTRIBUTE_UNUSED_LABEL
  if ((! reload_completed))
    {
      return gen_split_300 (operands);
    }
  goto ret0;

 L2023: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode
      && GET_CODE (x2) == PLUS)
    goto L2024;
  goto ret0;

 L2024: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == PLUS)
    goto L2025;
  goto ret0;

 L2025: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode
      && GET_CODE (x4) == MULT)
    goto L2026;
  goto ret0;

 L2026: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, DImode))
    {
      operands[1] = x5;
      goto L2027;
    }
  goto ret0;

 L2027: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (register_operand (x5, DImode))
    {
      operands[2] = x5;
      goto L2028;
    }
  goto ret0;

 L2028: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DImode))
    {
      operands[3] = x4;
      goto L2029;
    }
  goto ret0;

 L2029: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (gr_reg_or_14bit_operand (x3, DImode))
    {
      operands[4] = x3;
      goto L2030;
    }
  goto ret0;

 L2030: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2031;
  goto ret0;

 L2031: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[5] = x2;
      goto L2032;
    }
  goto ret0;

 L2032: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_322 (operands);
    }
  goto ret0;

 L5453: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, BImode))
    {
      operands[0] = x2;
      goto L1941;
    }
  goto ret0;

 L1941: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == NOT)
    goto L1942;
  goto ret0;

 L1942: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, BImode))
    {
      operands[1] = x3;
      goto L1943;
    }
  goto ret0;

 L1943: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L1944;
  goto ret0;

 L1944: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[2] = x2;
      goto L1945;
    }
  goto ret0;

 L1945: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && rtx_equal_p (operands[0], operands[1])))
    {
      return gen_split_305 (operands);
    }
 L1953: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed
   && GET_CODE (operands[0]) == REG && PR_REGNO_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && PR_REGNO_P (REGNO (operands[1]))
   && ! rtx_equal_p (operands[0], operands[1])))
    {
      return gen_split_306 (operands);
    }
  goto ret0;

 L2005: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L2006;
  goto ret0;

 L2006: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5455;
    case DFmode:
      goto L5456;
    default:
      break;
    }
  goto ret0;

 L5455: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2007;
    }
  goto ret0;

 L2007: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == FLOAT)
    goto L2008;
  goto ret0;

 L2008: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L2009;
  goto ret0;

 L2009: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2010;
    }
  goto ret0;

 L2010: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L2011;
    }
  goto ret0;

 L2011: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2012;
  goto ret0;

 L2012: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TFmode)
    goto L5457;
  goto ret0;

 L5457: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2013;
    }
 L5458: ATTRIBUTE_UNUSED_LABEL
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2042;
    }
  goto ret0;

 L2013: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2014;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5458;

 L2014: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L2015;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5458;

 L2015: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2016;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5458;

 L2016: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2017;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5458;

 L2017: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == USE)
    goto L2018;
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5458;

 L2018: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2019;
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5458;

 L2019: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV&& reload_completed))
    {
      return gen_split_321 (operands);
    }
  x1 = XVECEXP (x0, 0, 1);
  x2 = XEXP (x1, 0);
  goto L5458;

 L2042: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2043;
  goto ret0;

 L2043: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2044;
    }
  goto ret0;

 L2044: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2045;
  goto ret0;

 L2045: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L2046;
    }
  goto ret0;

 L2046: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L2047;
  goto ret0;

 L2047: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2048;
    }
  goto ret0;

 L2048: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_LAT&& reload_completed))
    {
      return gen_split_333 (operands);
    }
  goto ret0;

 L5456: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2092;
    }
  goto ret0;

 L2092: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L2093;
  goto ret0;

 L2093: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L2094;
    }
  goto ret0;

 L2094: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L2095;
    }
  goto ret0;

 L2095: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2096;
  goto ret0;

 L2096: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2097;
    }
  goto ret0;

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
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed))
    {
      return gen_split_339 (operands);
    }
  goto ret0;

 L2050: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    case SET:
      goto L2051;
    case CALL:
      goto L2252;
    default:
     break;
   }
  goto ret0;

 L2051: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    case TFmode:
      goto L5459;
    case SFmode:
      goto L5460;
    case DFmode:
      goto L5461;
    default:
      break;
    }
 L2276: ATTRIBUTE_UNUSED_LABEL
  operands[0] = x2;
  goto L2277;

 L5459: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2052;
    }
  goto L2276;

 L2052: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode)
    goto L5462;
  x2 = XEXP (x1, 0);
  goto L2276;

 L5462: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    case FLOAT:
      goto L2053;
    case DIV:
      goto L2138;
    default:
     break;
   }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2053: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode
      && GET_CODE (x3) == DIV)
    goto L2054;
  x2 = XEXP (x1, 0);
  goto L2276;

 L2054: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (fr_register_operand (x4, TFmode))
    {
      operands[1] = x4;
      goto L2055;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2055: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (fr_register_operand (x4, TFmode))
    {
      operands[2] = x4;
      goto L2056;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2056: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2057;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2057: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2058;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2058: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2059;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2059: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2060;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2060: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2061;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2061: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2062;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2062: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_INT_DIV_THR&& reload_completed))
    {
      return gen_split_334 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2138: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2139;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2139: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L2140;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2140: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2141;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2141: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2142;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2142: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2143;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2143: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2144;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2144: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2145;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2145: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2146;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2146: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed))
    {
      return gen_split_343 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L5460: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, SFmode))
    {
      operands[0] = x2;
      goto L2066;
    }
  goto L2276;

 L2066: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SFmode
      && GET_CODE (x2) == DIV)
    goto L2067;
  x2 = XEXP (x1, 0);
  goto L2276;

 L2067: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, SFmode))
    {
      operands[1] = x3;
      goto L2068;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2068: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, SFmode))
    {
      operands[2] = x3;
      goto L2069;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2069: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2070;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2070: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2071;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2071: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2072;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2072: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2073;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2073: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2074;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2074: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2075;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2075: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed))
    {
      return gen_split_336 (operands);
    }
 L2088: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed))
    {
      return gen_split_337 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L5461: ATTRIBUTE_UNUSED_LABEL
  if (fr_register_operand (x2, DFmode))
    {
      operands[0] = x2;
      goto L2107;
    }
  goto L2276;

 L2107: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DFmode
      && GET_CODE (x2) == DIV)
    goto L2108;
  x2 = XEXP (x1, 0);
  goto L2276;

 L2108: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, DFmode))
    {
      operands[1] = x3;
      goto L2109;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2109: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, DFmode))
    {
      operands[2] = x3;
      goto L2110;
    }
  x2 = XEXP (x1, 0);
  goto L2276;

 L2110: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2111;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2111: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2112;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2112: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2113;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2113: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      operands[4] = x2;
      goto L2114;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2114: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2115;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2115: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[5] = x2;
      goto L2116;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2116: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_THR&& reload_completed))
    {
      return gen_split_340 (operands);
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2276;

 L2277: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL)
    goto L2278;
  goto ret0;

 L2278: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode
      && GET_CODE (x3) == MEM)
    goto L2279;
  goto ret0;

 L2279: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_operand (x4, DImode))
    {
      operands[1] = x4;
      goto L2280;
    }
  goto ret0;

 L2280: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT
      && XWINT (x3, 0) == 1LL)
    goto L2281;
  goto ret0;

 L2281: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2282;
  goto ret0;

 L2282: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2283;
    }
  goto ret0;

 L2283: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2284;
  goto ret0;

 L2284: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2285;
    }
  goto ret0;

 L2285: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2286;
  goto ret0;

 L2286: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[4] = x2;
      goto L2287;
    }
  goto ret0;

 L2287: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && find_reg_note (insn, REG_NORETURN, NULL_RTX)))
    {
      return gen_split_401 (operands);
    }
 L2301: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_402 (operands);
    }
  goto ret0;

 L2252: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == MEM)
    goto L2253;
  goto ret0;

 L2253: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_operand (x3, VOIDmode))
    {
      operands[0] = x3;
      goto L2254;
    }
  goto ret0;

 L2254: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT
      && XWINT (x2, 0) == 1LL)
    goto L2255;
  goto ret0;

 L2255: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2256;
  goto ret0;

 L2256: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      operands[1] = x2;
      goto L2257;
    }
  goto ret0;

 L2257: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2258;
  goto ret0;

 L2258: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[2] = x2;
      goto L2259;
    }
  goto ret0;

 L2259: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2260;
  goto ret0;

 L2260: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      operands[3] = x2;
      goto L2261;
    }
  goto ret0;

 L2261: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed && find_reg_note (insn, REG_NORETURN, NULL_RTX)))
    {
      return gen_split_399 (operands);
    }
 L2273: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_400 (operands);
    }
  goto ret0;

 L2118: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET)
    goto L2119;
  goto ret0;

 L2119: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (fr_register_operand (x2, TFmode))
    {
      operands[0] = x2;
      goto L2120;
    }
  goto ret0;

 L2120: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == TFmode
      && GET_CODE (x2) == DIV)
    goto L2121;
  goto ret0;

 L2121: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (fr_register_operand (x3, TFmode))
    {
      operands[1] = x3;
      goto L2122;
    }
  goto ret0;

 L2122: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (fr_register_operand (x3, TFmode))
    {
      operands[2] = x3;
      goto L2123;
    }
  goto ret0;

 L2123: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER)
    goto L2124;
  goto ret0;

 L2124: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[3] = x2;
      goto L2125;
    }
  goto ret0;

 L2125: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER)
    goto L2126;
  goto ret0;

 L2126: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[4] = x2;
      goto L2127;
    }
  goto ret0;

 L2127: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER)
    goto L2128;
  goto ret0;

 L2128: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[5] = x2;
      goto L2129;
    }
  goto ret0;

 L2129: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER)
    goto L2130;
  goto ret0;

 L2130: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, TFmode))
    {
      operands[6] = x2;
      goto L2131;
    }
  goto ret0;

 L2131: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == CLOBBER)
    goto L2132;
  goto ret0;

 L2132: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, BImode))
    {
      operands[7] = x2;
      goto L2133;
    }
  goto ret0;

 L2133: ATTRIBUTE_UNUSED_LABEL
  if ((INTEL_EXTENDED_IEEE_FORMAT && TARGET_INLINE_FLOAT_DIV_LAT&& reload_completed))
    {
      return gen_split_342 (operands);
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
      goto L1854;
    case PARALLEL:
      goto L5414;
    case UNSPEC_VOLATILE:
      goto L5419;
    default:
     break;
   }
  goto ret0;

 L1854: ATTRIBUTE_UNUSED_LABEL
  return split_1 (x0, insn);

 L5414: ATTRIBUTE_UNUSED_LABEL
  return split_2 (x0, insn);

 L5419: ATTRIBUTE_UNUSED_LABEL
  if (XVECLEN (x0, 0) == 1
      && XINT (x0, 1) == 7)
    goto L2313;
  goto ret0;

 L2313: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  operands[0] = x1;
  goto L2314;

 L2314: ATTRIBUTE_UNUSED_LABEL
  if ((reload_completed))
    {
      return gen_split_414 (operands);
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
    goto L1992;
  goto ret0;

 L1992: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (register_operand (x1, BImode))
    {
      operands[0] = x1;
      goto L1993;
    }
  goto ret0;

 L1993: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (comparison_operator (x1, BImode))
    {
      operands[1] = x1;
      goto L1994;
    }
  goto ret0;

 L1994: ATTRIBUTE_UNUSED_LABEL
  tem = peep2_next_insn (1);
  if (tem == NULL_RTX)
    goto ret0;
  x1 = PATTERN (tem);
  if (GET_CODE (x1) == SET)
    goto L1995;
  goto ret0;

 L1995: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, CCImode))
    {
      operands[2] = x2;
      goto L1996;
    }
  goto ret0;

 L1996: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, CCImode))
    {
      operands[3] = x2;
      goto L1997;
    }
  goto ret0;

 L1997: ATTRIBUTE_UNUSED_LABEL
  tem = peep2_next_insn (2);
  if (tem == NULL_RTX)
    goto ret0;
  x1 = PATTERN (tem);
  if (GET_CODE (x1) == SET)
    goto L1998;
  goto ret0;

 L1998: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, CCImode))
    {
      operands[4] = x2;
      goto L1999;
    }
  goto ret0;

 L1999: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, CCImode))
    {
      operands[5] = x2;
      goto L2000;
    }
  goto ret0;

 L2000: ATTRIBUTE_UNUSED_LABEL
  tem = peep2_next_insn (3);
  if (tem == NULL_RTX)
    goto ret0;
  x1 = PATTERN (tem);
  if (GET_CODE (x1) == SET)
    goto L2001;
  goto ret0;

 L2001: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, BImode))
    {
      operands[6] = x2;
      goto L2002;
    }
  goto ret0;

 L2002: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BImode
      && GET_CODE (x2) == UNSPEC
      && XVECLEN (x2, 0) == 1
      && XINT (x2, 1) == 15)
    goto L2003;
  goto ret0;

 L2003: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (rtx_equal_p (x3, operands[6])
      && (REGNO (operands[3]) == REGNO (operands[0])
   && REGNO (operands[4]) == REGNO (operands[0]) + 1
   && REGNO (operands[4]) == REGNO (operands[2]) + 1
   && REGNO (operands[6]) == REGNO (operands[2])))
    {
      *_pmatch_len = 3;
      tem = gen_peephole2_311 (insn, operands);
      if (tem != 0)
        return tem;
    }
  goto ret0;
 ret0:
  return 0;
}

