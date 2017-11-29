/* Generated automatically by the program `genattrtab'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "regs.h"
#include "real.h"
#include "output.h"
#include "insn-attr.h"
#include "toplev.h"
#include "flags.h"
#include "function.h"

#define operands recog_data.operand

extern int bypass_p PARAMS ((rtx));
int
bypass_p (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int insn_default_latency PARAMS ((rtx));
int
insn_default_latency (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int insn_alts PARAMS ((rtx));
int
insn_alts (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int internal_dfa_insn_code PARAMS ((rtx));
int
internal_dfa_insn_code (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

extern int result_ready_cost PARAMS ((rtx));
int
result_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 424:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 6)
        {
	  return 9;
        }
      else if (which_alternative == 8)
        {
	  return 6;
        }
      else if (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 5;
        }
      else if ((which_alternative == 5) || (which_alternative == 3))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
      return 11 /* 0xb */;

    case 640:
    case 246:
      return 13 /* 0xd */;

    case 639:
    case 245:
      return 9;

    case 596:
    case 189:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 528:
    case 527:
    case 525:
    case 524:
    case 517:
    case 516:
    case 471:
    case 470:
    case 469:
    case 468:
    case 467:
    case 466:
    case 465:
    case 464:
    case 463:
    case 462:
    case 461:
    case 460:
    case 459:
    case 458:
    case 112:
    case 111:
    case 109:
    case 108:
    case 100:
    case 99:
    case 54:
    case 53:
    case 52:
    case 51:
    case 50:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 44:
    case 43:
    case 42:
    case 41:
      return 7;

    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 586:
    case 585:
    case 584:
    case 583:
    case 582:
    case 581:
    case 580:
    case 579:
    case 578:
    case 577:
    case 576:
    case 575:
    case 574:
    case 573:
    case 572:
    case 571:
    case 570:
    case 569:
    case 568:
    case 567:
    case 566:
    case 565:
    case 564:
    case 563:
    case 562:
    case 561:
    case 560:
    case 559:
    case 558:
    case 557:
    case 556:
    case 555:
    case 554:
    case 553:
    case 552:
    case 551:
    case 550:
    case 549:
    case 548:
    case 547:
    case 546:
    case 545:
    case 544:
    case 543:
    case 542:
    case 541:
    case 540:
    case 539:
    case 538:
    case 537:
    case 536:
    case 535:
    case 534:
    case 533:
    case 532:
    case 531:
    case 457:
    case 456:
    case 455:
    case 454:
    case 453:
    case 452:
    case 188:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 179:
    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 172:
    case 171:
    case 170:
    case 169:
    case 168:
    case 167:
    case 166:
    case 165:
    case 164:
    case 163:
    case 162:
    case 161:
    case 160:
    case 159:
    case 158:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 149:
    case 148:
    case 147:
    case 146:
    case 145:
    case 144:
    case 143:
    case 142:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 118:
    case 117:
    case 40:
    case 39:
    case 38:
    case 37:
    case 36:
    case 35:
      return 5;

    case 451:
    case 34:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return 5;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 599:
    case 450:
    case 449:
    case 192:
    case 33:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 610:
    case 609:
    case 608:
    case 607:
    case 448:
    case 203:
    case 202:
    case 201:
    case 200:
    case 31:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 5;
        }
      else
        {
	  return 1;
        }

    case 445:
    case 28:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 9;
        }
      else if (which_alternative == 0)
        {
	  return 5;
        }
      else
        {
	  return 1;
        }

    case 444:
    case 443:
    case 27:
    case 26:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || (which_alternative == 4))
        {
	  return 9;
        }
      else if (which_alternative == 0)
        {
	  return 5;
        }
      else if ((which_alternative == 3) || (which_alternative == 6))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 637:
    case 618:
    case 617:
    case 616:
    case 603:
    case 602:
    case 530:
    case 509:
    case 479:
    case 478:
    case 477:
    case 431:
    case 243:
    case 211:
    case 210:
    case 209:
    case 196:
    case 195:
    case 114:
    case 92:
    case 62:
    case 61:
    case 60:
    case 12:
      return 2;

    case 426:
    case 7:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 12)
        {
	  return 13 /* 0xd */;
        }
      else if ((which_alternative == 8) || (which_alternative == 6))
        {
	  return 9;
        }
      else if (which_alternative == 14)
        {
	  return 6;
        }
      else if ((which_alternative == 7) || (which_alternative == 15))
        {
	  return 5;
        }
      else if ((which_alternative == 10) || ((which_alternative == 5) || ((which_alternative == 16) || (which_alternative == 3))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 5:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 6)
        {
	  return 9;
        }
      else if (which_alternative == 8)
        {
	  return 6;
        }
      else if ((which_alternative == 7) || (which_alternative == 9))
        {
	  return 5;
        }
      else if ((which_alternative == 5) || (which_alternative == 3))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 422:
    case 421:
    case 3:
    case 2:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 5)
        {
	  return 9;
        }
      else if (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 5;
        }
      else if ((which_alternative == 4) || (which_alternative == 2))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 420:
    case 1:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 6)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

extern int stop_bit_unit_ready_cost PARAMS ((rtx));
int
stop_bit_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

extern int dummy_unit_ready_cost PARAMS ((rtx));
int
dummy_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 424:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 6)
        {
	  return 9;
        }
      else if (which_alternative == 8)
        {
	  return 6;
        }
      else if (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 5;
        }
      else if ((which_alternative == 3) || (which_alternative == 5))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
      return 11 /* 0xb */;

    case 649:
    case 647:
    case 640:
    case 261:
    case 259:
    case 258:
    case 257:
    case 246:
      return 13 /* 0xd */;

    case 639:
    case 245:
      return 9;

    case 596:
    case 189:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 528:
    case 527:
    case 525:
    case 524:
    case 517:
    case 516:
    case 471:
    case 470:
    case 469:
    case 468:
    case 467:
    case 466:
    case 465:
    case 464:
    case 463:
    case 462:
    case 461:
    case 460:
    case 459:
    case 458:
    case 112:
    case 111:
    case 109:
    case 108:
    case 100:
    case 99:
    case 54:
    case 53:
    case 52:
    case 51:
    case 50:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 44:
    case 43:
    case 42:
    case 41:
      return 7;

    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 586:
    case 585:
    case 584:
    case 583:
    case 582:
    case 581:
    case 580:
    case 579:
    case 578:
    case 577:
    case 576:
    case 575:
    case 574:
    case 573:
    case 572:
    case 571:
    case 570:
    case 569:
    case 568:
    case 567:
    case 566:
    case 565:
    case 564:
    case 563:
    case 562:
    case 561:
    case 560:
    case 559:
    case 558:
    case 557:
    case 556:
    case 555:
    case 554:
    case 553:
    case 552:
    case 551:
    case 550:
    case 549:
    case 548:
    case 547:
    case 546:
    case 545:
    case 544:
    case 543:
    case 542:
    case 541:
    case 540:
    case 539:
    case 538:
    case 537:
    case 536:
    case 535:
    case 534:
    case 533:
    case 532:
    case 531:
    case 457:
    case 456:
    case 455:
    case 454:
    case 453:
    case 452:
    case 188:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 179:
    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 172:
    case 171:
    case 170:
    case 169:
    case 168:
    case 167:
    case 166:
    case 165:
    case 164:
    case 163:
    case 162:
    case 161:
    case 160:
    case 159:
    case 158:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 149:
    case 148:
    case 147:
    case 146:
    case 145:
    case 144:
    case 143:
    case 142:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 118:
    case 117:
    case 40:
    case 39:
    case 38:
    case 37:
    case 36:
    case 35:
      return 5;

    case 451:
    case 34:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return 5;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 599:
    case 450:
    case 449:
    case 192:
    case 33:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 610:
    case 609:
    case 608:
    case 607:
    case 448:
    case 203:
    case 202:
    case 201:
    case 200:
    case 31:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 5;
        }
      else
        {
	  return 1;
        }

    case 445:
    case 28:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 9;
        }
      else if (which_alternative == 0)
        {
	  return 5;
        }
      else
        {
	  return 1;
        }

    case 444:
    case 443:
    case 27:
    case 26:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) || (which_alternative == 1))
        {
	  return 9;
        }
      else if (which_alternative == 0)
        {
	  return 5;
        }
      else if ((which_alternative == 6) || (which_alternative == 3))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 637:
    case 618:
    case 617:
    case 616:
    case 603:
    case 602:
    case 530:
    case 509:
    case 479:
    case 478:
    case 477:
    case 431:
    case 243:
    case 211:
    case 210:
    case 209:
    case 196:
    case 195:
    case 114:
    case 92:
    case 62:
    case 61:
    case 60:
    case 12:
      return 2;

    case 426:
    case 7:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 12)
        {
	  return 13 /* 0xd */;
        }
      else if ((which_alternative == 6) || (which_alternative == 8))
        {
	  return 9;
        }
      else if (which_alternative == 14)
        {
	  return 6;
        }
      else if ((which_alternative == 15) || (which_alternative == 7))
        {
	  return 5;
        }
      else if ((which_alternative == 3) || ((which_alternative == 16) || ((which_alternative == 5) || (which_alternative == 10))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 5:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 6)
        {
	  return 9;
        }
      else if (which_alternative == 8)
        {
	  return 6;
        }
      else if ((which_alternative == 9) || (which_alternative == 7))
        {
	  return 5;
        }
      else if ((which_alternative == 3) || (which_alternative == 5))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 422:
    case 421:
    case 3:
    case 2:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 5)
        {
	  return 9;
        }
      else if (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 5;
        }
      else if ((which_alternative == 2) || (which_alternative == 4))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 420:
    case 1:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 6)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

extern int function_units_used PARAMS ((rtx));
int
function_units_used (insn)
     rtx insn;
{
  enum attr_itanium_class attr_itanium_class = get_attr_itanium_class (insn);
  unsigned long accum = 0;

  accum |= ((attr_itanium_class == ITANIUM_CLASS_STOP_BIT) ? (2) : (0));
  accum |= (((attr_itanium_class == ITANIUM_CLASS_BR) || ((attr_itanium_class == ITANIUM_CLASS_SCALL) || ((attr_itanium_class == ITANIUM_CLASS_FCMP) || ((attr_itanium_class == ITANIUM_CLASS_FCVTFX) || ((attr_itanium_class == ITANIUM_CLASS_FLD) || ((attr_itanium_class == ITANIUM_CLASS_FMAC) || ((attr_itanium_class == ITANIUM_CLASS_FMISC) || ((attr_itanium_class == ITANIUM_CLASS_FRAR_I) || ((attr_itanium_class == ITANIUM_CLASS_FRAR_M) || ((attr_itanium_class == ITANIUM_CLASS_FRBR) || ((attr_itanium_class == ITANIUM_CLASS_FRFR) || ((attr_itanium_class == ITANIUM_CLASS_FRPR) || ((attr_itanium_class == ITANIUM_CLASS_IALU) || ((attr_itanium_class == ITANIUM_CLASS_ICMP) || ((attr_itanium_class == ITANIUM_CLASS_ILOG) || ((attr_itanium_class == ITANIUM_CLASS_ISHF) || ((attr_itanium_class == ITANIUM_CLASS_LD) || ((attr_itanium_class == ITANIUM_CLASS_LONG_I) || ((attr_itanium_class == ITANIUM_CLASS_MMMUL) || ((attr_itanium_class == ITANIUM_CLASS_MMSHF) || ((attr_itanium_class == ITANIUM_CLASS_MMSHFI) || ((attr_itanium_class == ITANIUM_CLASS_RSE_M) || ((attr_itanium_class == ITANIUM_CLASS_SEM) || ((attr_itanium_class == ITANIUM_CLASS_STF) || ((attr_itanium_class == ITANIUM_CLASS_ST) || ((attr_itanium_class == ITANIUM_CLASS_SYST_M0) || ((attr_itanium_class == ITANIUM_CLASS_SYST_M) || ((attr_itanium_class == ITANIUM_CLASS_TBIT) || ((attr_itanium_class == ITANIUM_CLASS_TOAR_I) || ((attr_itanium_class == ITANIUM_CLASS_TOAR_M) || ((attr_itanium_class == ITANIUM_CLASS_TOBR) || ((attr_itanium_class == ITANIUM_CLASS_TOFR) || ((attr_itanium_class == ITANIUM_CLASS_TOPR) || ((attr_itanium_class == ITANIUM_CLASS_XMPY) || ((attr_itanium_class == ITANIUM_CLASS_XTD) || ((attr_itanium_class == ITANIUM_CLASS_NOP_M) || ((attr_itanium_class == ITANIUM_CLASS_NOP_I) || ((attr_itanium_class == ITANIUM_CLASS_NOP_F) || ((attr_itanium_class == ITANIUM_CLASS_NOP_B) || ((attr_itanium_class == ITANIUM_CLASS_NOP_X) || ((attr_itanium_class == ITANIUM_CLASS_IGNORE) || (attr_itanium_class == ITANIUM_CLASS_UNKNOWN)))))))))))))))))))))))))))))))))))))))))) ? (1) : (0));

  if (accum && accum == (accum & -accum))
    {
      int i;
      for (i = 0; accum >>= 1; ++i) continue;
      accum = i;
    }
  else
    accum = ~accum;
  return accum;
}

extern enum attr_itanium_requires_unit0 get_attr_itanium_requires_unit0 PARAMS ((rtx));
enum attr_itanium_requires_unit0
get_attr_itanium_requires_unit0 (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 424:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 6)))))
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 596:
    case 482:
    case 481:
    case 480:
    case 189:
    case 65:
    case 64:
    case 63:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 451:
    case 34:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 610:
    case 609:
    case 608:
    case 607:
    case 483:
    case 448:
    case 203:
    case 202:
    case 201:
    case 200:
    case 66:
    case 31:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 484:
    case 445:
    case 67:
    case 28:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 444:
    case 443:
    case 27:
    case 26:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || (which_alternative == 0))
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 426:
    case 7:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 5) || ((which_alternative == 15) || ((which_alternative == 14) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 17) || ((which_alternative == 16) || ((which_alternative == 13) || ((which_alternative == 12) || (which_alternative == 7))))))))))
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 5:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 5) || ((which_alternative == 9) || ((which_alternative == 8) || (which_alternative == 7))))
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 422:
    case 421:
    case 3:
    case 2:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 5))))
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 420:
    case 1:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  return ITANIUM_REQUIRES_UNIT0_YES;
        }
      else
        {
	  return ITANIUM_REQUIRES_UNIT0_NO;
        }

    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 640:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 605:
    case 604:
    case 579:
    case 578:
    case 577:
    case 576:
    case 575:
    case 553:
    case 552:
    case 551:
    case 550:
    case 549:
    case 538:
    case 537:
    case 536:
    case 535:
    case 534:
    case 530:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 496:
    case 495:
    case 494:
    case 493:
    case 475:
    case 474:
    case 473:
    case 472:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 248:
    case 246:
    case 241:
    case 213:
    case 212:
    case 211:
    case 210:
    case 209:
    case 198:
    case 197:
    case 188:
    case 169:
    case 168:
    case 167:
    case 166:
    case 165:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 114:
    case 92:
    case 91:
    case 90:
    case 89:
    case 88:
    case 79:
    case 78:
    case 77:
    case 76:
    case 58:
    case 57:
    case 56:
    case 55:
      return ITANIUM_REQUIRES_UNIT0_YES;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return ITANIUM_REQUIRES_UNIT0_NO;

    }
}

extern enum attr_itanium_class get_attr_itanium_class PARAMS ((rtx));
enum attr_itanium_class
get_attr_itanium_class (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 426:
    case 7:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ITANIUM_CLASS_IALU;
        }
      else if (which_alternative == 2)
        {
	  return ITANIUM_CLASS_LONG_I;
        }
      else if (which_alternative == 3)
        {
	  return ITANIUM_CLASS_LD;
        }
      else if (which_alternative == 4)
        {
	  return ITANIUM_CLASS_ST;
        }
      else if (which_alternative == 5)
        {
	  return ITANIUM_CLASS_FRFR;
        }
      else if (which_alternative == 6)
        {
	  return ITANIUM_CLASS_TOFR;
        }
      else if (which_alternative == 7)
        {
	  return ITANIUM_CLASS_FMISC;
        }
      else if (which_alternative == 8)
        {
	  return ITANIUM_CLASS_FLD;
        }
      else if (which_alternative == 9)
        {
	  return ITANIUM_CLASS_STF;
        }
      else if (which_alternative == 10)
        {
	  return ITANIUM_CLASS_FRBR;
        }
      else if (which_alternative == 11)
        {
	  return ITANIUM_CLASS_TOBR;
        }
      else if (which_alternative == 12)
        {
	  return ITANIUM_CLASS_FRAR_I;
        }
      else if (which_alternative == 13)
        {
	  return ITANIUM_CLASS_TOAR_I;
        }
      else if (which_alternative == 14)
        {
	  return ITANIUM_CLASS_FRAR_M;
        }
      else if (which_alternative == 15)
        {
	  return ITANIUM_CLASS_TOAR_M;
        }
      else if (which_alternative == 16)
        {
	  return ITANIUM_CLASS_FRPR;
        }
      else
        {
	  return ITANIUM_CLASS_TOPR;
        }

    case 424:
    case 5:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ITANIUM_CLASS_IALU;
        }
      else if (which_alternative == 2)
        {
	  return ITANIUM_CLASS_LONG_I;
        }
      else if (which_alternative == 3)
        {
	  return ITANIUM_CLASS_LD;
        }
      else if (which_alternative == 4)
        {
	  return ITANIUM_CLASS_ST;
        }
      else if (which_alternative == 5)
        {
	  return ITANIUM_CLASS_FRFR;
        }
      else if (which_alternative == 6)
        {
	  return ITANIUM_CLASS_TOFR;
        }
      else if (which_alternative == 7)
        {
	  return ITANIUM_CLASS_FMISC;
        }
      else if (which_alternative == 8)
        {
	  return ITANIUM_CLASS_FRAR_M;
        }
      else
        {
	  return ITANIUM_CLASS_TOAR_M;
        }

    case 422:
    case 421:
    case 3:
    case 2:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ITANIUM_CLASS_IALU;
        }
      else if (which_alternative == 2)
        {
	  return ITANIUM_CLASS_LD;
        }
      else if (which_alternative == 3)
        {
	  return ITANIUM_CLASS_ST;
        }
      else if (which_alternative == 4)
        {
	  return ITANIUM_CLASS_FRFR;
        }
      else if (which_alternative == 5)
        {
	  return ITANIUM_CLASS_TOFR;
        }
      else
        {
	  return ITANIUM_CLASS_FMISC;
        }

    case 420:
    case 1:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ITANIUM_CLASS_ICMP;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return ITANIUM_CLASS_UNKNOWN;
        }
      else if (which_alternative == 4)
        {
	  return ITANIUM_CLASS_TBIT;
        }
      else if (which_alternative == 5)
        {
	  return ITANIUM_CLASS_IALU;
        }
      else if (which_alternative == 6)
        {
	  return ITANIUM_CLASS_LD;
        }
      else if (which_alternative == 7)
        {
	  return ITANIUM_CLASS_ST;
        }
      else
        {
	  return ITANIUM_CLASS_IALU;
        }

    case 220:
    case 222:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_IALU;
        }
      else
        {
	  return ITANIUM_CLASS_UNKNOWN;
        }

    case 26:
    case 27:
    case 443:
    case 444:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_FMISC;
        }
      else if (which_alternative == 1)
        {
	  return ITANIUM_CLASS_FLD;
        }
      else if (which_alternative == 2)
        {
	  return ITANIUM_CLASS_STF;
        }
      else if (which_alternative == 3)
        {
	  return ITANIUM_CLASS_FRFR;
        }
      else if (which_alternative == 4)
        {
	  return ITANIUM_CLASS_TOFR;
        }
      else if (which_alternative == 5)
        {
	  return ITANIUM_CLASS_IALU;
        }
      else if (which_alternative == 6)
        {
	  return ITANIUM_CLASS_LD;
        }
      else
        {
	  return ITANIUM_CLASS_ST;
        }

    case 28:
    case 445:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_FMISC;
        }
      else if (which_alternative == 1)
        {
	  return ITANIUM_CLASS_FLD;
        }
      else
        {
	  return ITANIUM_CLASS_STF;
        }

    case 31:
    case 448:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_XTD;
        }
      else
        {
	  return ITANIUM_CLASS_FMISC;
        }

    case 32:
    case 33:
    case 449:
    case 450:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_XTD;
        }
      else
        {
	  return ITANIUM_CLASS_LD;
        }

    case 34:
    case 451:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_XTD;
        }
      else if (which_alternative == 1)
        {
	  return ITANIUM_CLASS_LD;
        }
      else
        {
	  return ITANIUM_CLASS_FMISC;
        }

    case 63:
    case 64:
    case 65:
    case 480:
    case 481:
    case 482:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_UNKNOWN;
        }
      else if (which_alternative == 1)
        {
	  return ITANIUM_CLASS_TBIT;
        }
      else
        {
	  return ITANIUM_CLASS_ILOG;
        }

    case 66:
    case 483:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_UNKNOWN;
        }
      else
        {
	  return ITANIUM_CLASS_TBIT;
        }

    case 67:
    case 484:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_TBIT;
        }
      else if (which_alternative == 1)
        {
	  return ITANIUM_CLASS_ILOG;
        }
      else if (which_alternative == 2)
        {
	  return ITANIUM_CLASS_UNKNOWN;
        }
      else
        {
	  return ITANIUM_CLASS_UNKNOWN;
        }

    case 189:
    case 596:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_IALU;
        }
      else if (which_alternative == 1)
        {
	  return ITANIUM_CLASS_ISHF;
        }
      else
        {
	  return ITANIUM_CLASS_MMSHF;
        }

    case 192:
    case 599:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_IALU;
        }
      else if (which_alternative == 1)
        {
	  return ITANIUM_CLASS_MMSHF;
        }
      else
        {
	  return ITANIUM_CLASS_MMSHFI;
        }

    case 195:
    case 196:
    case 602:
    case 603:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_MMSHF;
        }
      else
        {
	  return ITANIUM_CLASS_MMSHFI;
        }

    case 200:
    case 201:
    case 202:
    case 203:
    case 607:
    case 608:
    case 609:
    case 610:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_ILOG;
        }
      else
        {
	  return ITANIUM_CLASS_FMISC;
        }

    case 225:
    case 226:
    case 227:
    case 228:
    case 229:
    case 623:
    case 624:
    case 625:
    case 626:
    case 627:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ITANIUM_CLASS_BR;
        }
      else
        {
	  return ITANIUM_CLASS_SCALL;
        }

    case 261:
    case 649:
      return ITANIUM_CLASS_LFETCH;

    case 254:
    case 646:
      return ITANIUM_CLASS_NOP_X;

    case 250:
    case 642:
      return ITANIUM_CLASS_NOP_M;

    case 251:
    case 643:
      return ITANIUM_CLASS_NOP_I;

    case 252:
    case 260:
    case 644:
    case 648:
      return ITANIUM_CLASS_NOP_F;

    case 253:
    case 645:
      return ITANIUM_CLASS_NOP_B;

    case 29:
    case 30:
    case 446:
    case 447:
      return ITANIUM_CLASS_XTD;

    case 99:
    case 100:
    case 108:
    case 109:
    case 111:
    case 112:
    case 516:
    case 517:
    case 524:
    case 525:
    case 527:
    case 528:
      return ITANIUM_CLASS_XMPY;

    case 76:
    case 77:
    case 78:
    case 79:
    case 88:
    case 89:
    case 90:
    case 91:
    case 212:
    case 213:
    case 493:
    case 494:
    case 495:
    case 496:
    case 505:
    case 506:
    case 507:
    case 508:
    case 619:
    case 620:
      return ITANIUM_CLASS_TBIT;

    case 263:
    case 651:
      return ITANIUM_CLASS_SYST_M;

    case 241:
      return ITANIUM_CLASS_SYST_M0;

    case 242:
    case 636:
      return ITANIUM_CLASS_ST;

    case 244:
    case 638:
      return ITANIUM_CLASS_STF;

    case 264:
    case 265:
    case 266:
    case 267:
    case 268:
    case 269:
    case 652:
    case 653:
    case 654:
    case 655:
    case 656:
    case 657:
      return ITANIUM_CLASS_SEM;

    case 248:
      return ITANIUM_CLASS_RSE_M;

    case 60:
    case 61:
    case 62:
    case 477:
    case 478:
    case 479:
      return ITANIUM_CLASS_MMSHF;

    case 92:
    case 114:
    case 509:
    case 530:
      return ITANIUM_CLASS_MMMUL;

    case 10:
    case 15:
    case 20:
    case 429:
    case 434:
    case 439:
      return ITANIUM_CLASS_LONG_I;

    case 258:
    case 259:
    case 647:
      return ITANIUM_CLASS_CHK_S;

    case 12:
    case 243:
    case 431:
    case 637:
      return ITANIUM_CLASS_LD;

    case 55:
    case 56:
    case 57:
    case 58:
    case 197:
    case 198:
    case 472:
    case 473:
    case 474:
    case 475:
    case 604:
    case 605:
      return ITANIUM_CLASS_ISHF;

    case 199:
    case 204:
    case 606:
    case 611:
      return ITANIUM_CLASS_ILOG;

    case 0:
    case 68:
    case 69:
    case 70:
    case 71:
    case 72:
    case 73:
    case 74:
    case 75:
    case 80:
    case 81:
    case 82:
    case 83:
    case 84:
    case 85:
    case 86:
    case 87:
    case 205:
    case 206:
    case 207:
    case 208:
    case 485:
    case 486:
    case 487:
    case 488:
    case 489:
    case 490:
    case 491:
    case 492:
    case 497:
    case 498:
    case 499:
    case 500:
    case 501:
    case 502:
    case 503:
    case 504:
    case 612:
    case 613:
    case 614:
    case 615:
      return ITANIUM_CLASS_ICMP;

    case 8:
    case 9:
    case 11:
    case 13:
    case 14:
    case 16:
    case 17:
    case 18:
    case 19:
    case 21:
    case 22:
    case 23:
    case 93:
    case 94:
    case 95:
    case 96:
    case 97:
    case 98:
    case 101:
    case 103:
    case 104:
    case 105:
    case 106:
    case 107:
    case 113:
    case 193:
    case 223:
    case 224:
    case 238:
    case 239:
    case 273:
    case 274:
    case 275:
    case 427:
    case 428:
    case 430:
    case 432:
    case 433:
    case 435:
    case 436:
    case 437:
    case 438:
    case 440:
    case 441:
    case 442:
    case 510:
    case 511:
    case 512:
    case 513:
    case 514:
    case 515:
    case 518:
    case 519:
    case 520:
    case 521:
    case 522:
    case 523:
    case 529:
    case 600:
    case 634:
    case 635:
    case 658:
    case 659:
    case 660:
      return ITANIUM_CLASS_IALU;

    case 246:
    case 640:
      return ITANIUM_CLASS_FRAR_I;

    case 120:
    case 121:
    case 122:
    case 123:
    case 124:
    case 137:
    case 138:
    case 139:
    case 140:
    case 141:
    case 165:
    case 166:
    case 167:
    case 168:
    case 169:
    case 188:
    case 534:
    case 535:
    case 536:
    case 537:
    case 538:
    case 549:
    case 550:
    case 551:
    case 552:
    case 553:
    case 575:
    case 576:
    case 577:
    case 578:
    case 579:
      return ITANIUM_CLASS_FMISC;

    case 245:
    case 639:
      return ITANIUM_CLASS_FLD;

    case 41:
    case 42:
    case 43:
    case 44:
    case 45:
    case 46:
    case 47:
    case 48:
    case 49:
    case 50:
    case 51:
    case 52:
    case 53:
    case 54:
    case 458:
    case 459:
    case 460:
    case 461:
    case 462:
    case 463:
    case 464:
    case 465:
    case 466:
    case 467:
    case 468:
    case 469:
    case 470:
    case 471:
      return ITANIUM_CLASS_FCVTFX;

    case 209:
    case 210:
    case 211:
    case 616:
    case 617:
    case 618:
      return ITANIUM_CLASS_FCMP;

    case 214:
    case 215:
    case 216:
    case 230:
    case 231:
    case 232:
    case 233:
    case 234:
    case 235:
    case 236:
    case 237:
    case 628:
    case 629:
    case 630:
    case 631:
    case 632:
    case 633:
      return ITANIUM_CLASS_BR;

    case 257:
      return ITANIUM_CLASS_STOP_BIT;

    case 240:
    case 255:
    case 256:
    case 270:
    case 271:
    case 272:
      return ITANIUM_CLASS_IGNORE;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 4:
    case 6:
    case 24:
    case 25:
    case 59:
    case 102:
    case 110:
    case 115:
    case 116:
    case 129:
    case 130:
    case 151:
    case 152:
    case 186:
    case 187:
    case 190:
    case 191:
    case 194:
    case 217:
    case 218:
    case 219:
    case 221:
    case 247:
    case 249:
    case 262:
    case 423:
    case 425:
    case 476:
    case 526:
    case 597:
    case 598:
    case 601:
    case 621:
    case 622:
    case 641:
    case 650:
      return ITANIUM_CLASS_UNKNOWN;

    default:
      return ITANIUM_CLASS_FMAC;

    }
}

extern enum attr_predicable get_attr_predicable PARAMS ((rtx));
enum attr_predicable
get_attr_predicable (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 0:
    case 24:
    case 25:
    case 102:
    case 115:
    case 116:
    case 129:
    case 130:
    case 151:
    case 152:
    case 186:
    case 187:
    case 188:
    case 214:
    case 215:
    case 216:
    case 219:
    case 220:
    case 221:
    case 222:
    case 223:
    case 224:
    case 233:
    case 234:
    case 240:
    case 241:
    case 247:
    case 248:
    case 255:
    case 256:
    case 257:
    case 259:
    case 270:
    case 271:
    case 272:
      return PREDICABLE_NO;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return PREDICABLE_YES;

    }
}

extern enum attr_type get_attr_type PARAMS ((rtx));
enum attr_type
get_attr_type (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 424:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 7))))
        {
	  return TYPE_M;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_A;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_F;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_L;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_M;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) || ((which_alternative == 0) || (which_alternative == 1)))
        {
	  return TYPE_A;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_I;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 222:
    case 220:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_A;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 610:
    case 609:
    case 608:
    case 607:
    case 203:
    case 202:
    case 201:
    case 200:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_A;
        }
      else
        {
	  return TYPE_F;
        }

    case 599:
    case 596:
    case 192:
    case 189:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_A;
        }
      else
        {
	  return TYPE_I;
        }

    case 484:
    case 67:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return TYPE_A;
        }
      else if (which_alternative == 0)
        {
	  return TYPE_I;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 483:
    case 66:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return TYPE_I;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 482:
    case 481:
    case 480:
    case 65:
    case 64:
    case 63:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return TYPE_A;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_I;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 451:
    case 34:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return TYPE_M;
        }
      else if (which_alternative != 0)
        {
	  return TYPE_F;
        }
      else
        {
	  return TYPE_I;
        }

    case 450:
    case 449:
    case 33:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return TYPE_M;
        }
      else
        {
	  return TYPE_I;
        }

    case 448:
    case 31:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return TYPE_F;
        }
      else
        {
	  return TYPE_I;
        }

    case 445:
    case 28:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return TYPE_M;
        }
      else
        {
	  return TYPE_F;
        }

    case 444:
    case 443:
    case 27:
    case 26:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5)))))) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return TYPE_M;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_A;
        }
      else if (which_alternative == 0)
        {
	  return TYPE_F;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 426:
    case 7:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 5) || (which_alternative == 6))))))))
        {
	  return TYPE_M;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_A;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_F;
        }
      else if ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 16) || (which_alternative == 17))))))
        {
	  return TYPE_I;
        }
      else
        {
	  return TYPE_L;
        }

    case 5:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 5) || (which_alternative == 6))))))
        {
	  return TYPE_M;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_A;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_F;
        }
      else
        {
	  return TYPE_L;
        }

    case 422:
    case 421:
    case 3:
    case 2:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))
        {
	  return TYPE_M;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_A;
        }
      else
        {
	  return TYPE_F;
        }

    case 1:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_M;
        }
      else if (((which_alternative == 5) || (which_alternative == 8)) || ((which_alternative == 0) || (which_alternative == 1)))
        {
	  return TYPE_A;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_I;
        }
      else
        {
	  return TYPE_UNKNOWN;
        }

    case 257:
      return TYPE_S;

    case 646:
    case 254:
      return TYPE_X;

    case 439:
    case 434:
    case 429:
    case 20:
    case 15:
    case 10:
      return TYPE_L;

    case 645:
    case 633:
    case 632:
    case 631:
    case 630:
    case 629:
    case 628:
    case 627:
    case 626:
    case 625:
    case 624:
    case 623:
    case 253:
    case 237:
    case 236:
    case 235:
    case 234:
    case 233:
    case 232:
    case 231:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 216:
    case 215:
    case 214:
      return TYPE_B;

    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 651:
    case 649:
    case 642:
    case 639:
    case 638:
    case 637:
    case 636:
    case 431:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 261:
    case 250:
    case 248:
    case 245:
    case 244:
    case 243:
    case 242:
    case 241:
    case 12:
      return TYPE_M;

    case 643:
    case 640:
    case 620:
    case 619:
    case 605:
    case 604:
    case 603:
    case 602:
    case 530:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 496:
    case 495:
    case 494:
    case 493:
    case 479:
    case 478:
    case 477:
    case 475:
    case 474:
    case 473:
    case 472:
    case 447:
    case 446:
    case 251:
    case 246:
    case 213:
    case 212:
    case 198:
    case 197:
    case 196:
    case 195:
    case 114:
    case 92:
    case 91:
    case 90:
    case 89:
    case 88:
    case 79:
    case 78:
    case 77:
    case 76:
    case 62:
    case 61:
    case 60:
    case 58:
    case 57:
    case 56:
    case 55:
    case 30:
    case 29:
      return TYPE_I;

    case 660:
    case 659:
    case 658:
    case 647:
    case 635:
    case 634:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 606:
    case 600:
    case 529:
    case 523:
    case 522:
    case 521:
    case 520:
    case 519:
    case 518:
    case 515:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 504:
    case 503:
    case 502:
    case 501:
    case 500:
    case 499:
    case 498:
    case 497:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 442:
    case 441:
    case 440:
    case 438:
    case 437:
    case 436:
    case 435:
    case 433:
    case 432:
    case 430:
    case 428:
    case 427:
    case 275:
    case 274:
    case 273:
    case 259:
    case 258:
    case 239:
    case 238:
    case 224:
    case 223:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 199:
    case 193:
    case 113:
    case 107:
    case 106:
    case 105:
    case 104:
    case 103:
    case 101:
    case 98:
    case 97:
    case 96:
    case 95:
    case 94:
    case 93:
    case 87:
    case 86:
    case 85:
    case 84:
    case 83:
    case 82:
    case 81:
    case 80:
    case 75:
    case 74:
    case 73:
    case 72:
    case 71:
    case 70:
    case 69:
    case 68:
    case 23:
    case 22:
    case 21:
    case 19:
    case 18:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 9:
    case 8:
    case 0:
      return TYPE_A;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 650:
    case 641:
    case 622:
    case 621:
    case 601:
    case 598:
    case 597:
    case 526:
    case 476:
    case 425:
    case 423:
    case 272:
    case 271:
    case 270:
    case 262:
    case 256:
    case 255:
    case 249:
    case 247:
    case 240:
    case 221:
    case 219:
    case 218:
    case 217:
    case 194:
    case 191:
    case 190:
    case 187:
    case 186:
    case 152:
    case 151:
    case 130:
    case 129:
    case 116:
    case 115:
    case 110:
    case 102:
    case 59:
    case 25:
    case 24:
    case 6:
    case 4:
      return TYPE_UNKNOWN;

    default:
      return TYPE_F;

    }
}

const struct function_unit_desc function_units[] = {
  {"dummy", 1, 6, 1, 1, 1, dummy_unit_ready_cost, 0, 1, 0, 0}, 
  {"stop_bit", 2, 1, 1, 1, 1, stop_bit_unit_ready_cost, 0, 1, 0, 0}, 
};


int max_dfa_issue_rate = 0;
/* Vector translating external insn codes to internal ones.*/
static const unsigned char translate_0[] ATTRIBUTE_UNUSED = {
    0};

/* Vector for state transitions.  */
static const unsigned char transitions_0[] ATTRIBUTE_UNUSED = {
    0};


#if AUTOMATON_STATE_ALTS
/* Vector for state insn alternatives.  */
static const unsigned char state_alts_0[] ATTRIBUTE_UNUSED = {
    1};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char min_issue_delay_0[] ATTRIBUTE_UNUSED = {
    0};

/* Vector for locked state flags.  */
static const unsigned char dead_lock_0[] = {
    1};


#define DFA__ADVANCE_CYCLE 0

struct DFA_chip
{
  unsigned char automaton_state_0;
};


int max_insn_queue_index = 1;

static int internal_min_issue_delay PARAMS ((int, struct DFA_chip *));
static int
internal_min_issue_delay (insn_code, chip)
	int insn_code;
	struct DFA_chip *chip  ATTRIBUTE_UNUSED;
{
  int temp ATTRIBUTE_UNUSED;
  int res = -1;

  switch (insn_code)
    {
    case 0: /* $advance_cycle */
      break;


    default:
      res = -1;
      break;
    }
  return res;
}

static int internal_state_transition PARAMS ((int, struct DFA_chip *));
static int
internal_state_transition (insn_code, chip)
	int insn_code;
	struct DFA_chip *chip  ATTRIBUTE_UNUSED;
{
  int temp ATTRIBUTE_UNUSED;

  switch (insn_code)
    {
    case 0: /* $advance_cycle */
      {
        return -1;
      }

    default:
      return -1;
    }
}


static int *dfa_insn_codes;

static int dfa_insn_codes_length;

#ifdef __GNUC__
__inline__
#endif
static int dfa_insn_code PARAMS ((rtx));
static int
dfa_insn_code (insn)
	rtx insn;
{
  int insn_code;
  int temp;

  if (INSN_UID (insn) >= dfa_insn_codes_length)
    {
      temp = dfa_insn_codes_length;
      dfa_insn_codes_length = 2 * INSN_UID (insn);
      dfa_insn_codes = xrealloc (dfa_insn_codes, dfa_insn_codes_length * sizeof (int));
      for (; temp < dfa_insn_codes_length; temp++)
        dfa_insn_codes [temp] = -1;
    }
  if ((insn_code = dfa_insn_codes [INSN_UID (insn)]) < 0)
    {
      insn_code = internal_dfa_insn_code (insn);
      dfa_insn_codes [INSN_UID (insn)] = insn_code;
    }
  return insn_code;
}

int
state_transition (state, insn)
	state_t state;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return -1;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;

  return internal_state_transition (insn_code, state);
}


#if AUTOMATON_STATE_ALTS

static int internal_state_alts PARAMS ((int, struct DFA_chip *));
static int
internal_state_alts (insn_code, chip)
	int insn_code;
	struct DFA_chip *chip;
{
  int res;

  switch (insn_code)
    {
    case 0: /* $advance_cycle */
      {
        break;
      }


    default:
      res = 0;
      break;
    }
  return res;
}

int
state_alts (state, insn)
	state_t state;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;

  return internal_state_alts (insn_code, state);
}


#endif /* #if AUTOMATON_STATE_ALTS */

int
min_issue_delay (state, insn)
	state_t state;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;

  return internal_min_issue_delay (insn_code, state);
}

static int internal_state_dead_lock_p PARAMS ((struct DFA_chip *));
static int
internal_state_dead_lock_p (chip)
	struct DFA_chip *chip;
{
  if (dead_lock_0 [chip->automaton_state_0])
    return 1/* TRUE */;
  return 0/* FALSE */;
}

int
state_dead_lock_p (state)
	state_t state;
{
  return internal_state_dead_lock_p (state);
}

int
state_size ()
{
  return sizeof (struct DFA_chip);
}

static void internal_reset PARAMS ((struct DFA_chip *));
static void
internal_reset (chip)
	struct DFA_chip *chip;
{
  memset (chip, 0, sizeof (struct DFA_chip));
}

void
state_reset (state)
	 state_t state;
{
  internal_reset (state);
}

int
min_insn_conflict_delay (state, insn, insn2)
	state_t state;
	rtx insn;
	rtx insn2;
{
  struct DFA_chip DFA_chip;
  int insn_code, insn2_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;


  if (insn2 != 0)
    {
      insn2_code = dfa_insn_code (insn2);
      if (insn2_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn2_code = DFA__ADVANCE_CYCLE;

  memcpy (&DFA_chip, state, sizeof (DFA_chip));
  internal_reset (&DFA_chip);
  if (internal_state_transition (insn_code, &DFA_chip) > 0)
    abort ();
  return internal_min_issue_delay (insn2_code, &DFA_chip);
}

static int internal_insn_latency PARAMS ((int, int, rtx, rtx));
static int
internal_insn_latency (insn_code, insn2_code, insn, insn2)
	int insn_code;
	int insn2_code;
	rtx insn ATTRIBUTE_UNUSED;
	rtx insn2 ATTRIBUTE_UNUSED;
{
  switch (insn_code)
    {
    case 0:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 0 : 0);
    default:
      return 0;
    }
}

int
insn_latency (insn, insn2)
	rtx insn;
	rtx insn2;
{
  int insn_code, insn2_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;


  if (insn2 != 0)
    {
      insn2_code = dfa_insn_code (insn2);
      if (insn2_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn2_code = DFA__ADVANCE_CYCLE;

  return internal_insn_latency (insn_code, insn2_code, insn, insn2);
}

void
print_reservation (f, insn)
	FILE *f;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        {
          fprintf (f, "nothing");
          return;
        }
    }
  else
    {
      fprintf (f, "nothing");
      return;
    }
  switch (insn_code)
    {
    default:
      fprintf (f, "nothing");
    }
}

void
dfa_start ()
{
  int i;

  dfa_insn_codes_length = get_max_uid ();
  dfa_insn_codes = (int *) xmalloc (dfa_insn_codes_length * sizeof (int));
  for (i = 0; i < dfa_insn_codes_length; i++)
    dfa_insn_codes [i] = -1;
}

void
dfa_finish ()
{
  free (dfa_insn_codes);
}

