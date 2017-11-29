/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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

extern int insn_current_length PARAMS ((rtx));
int
insn_current_length (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 520:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) >= (-126)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) < (128))))
        {
	  return 2;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 515:
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) >= (-126)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) < (128)))
        {
	  return 2;
        }
      else
        {
	  return 5;
        }

    case 504:
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) >= (-126)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) < (128)))
        {
	  return 2;
        }
      else
        {
	  return 6;
        }

    case 503:
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) >= (-126)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) < (128)))
        {
	  return 2;
        }
      else
        {
	  return 6;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int insn_variable_length_p PARAMS ((rtx));
int
insn_variable_length_p (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 520:
    case 515:
    case 504:
    case 503:
      return 1;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int insn_default_length PARAMS ((rtx));
int
insn_default_length (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 642:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }

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
    case 562:
    case 561:
    case 560:
    case 559:
    case 557:
    case 556:
      if (get_attr_unit (insn) == UNIT_I387)
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 585:
    case 582:
    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 496:
    case 495:
    case 493:
    case 484:
    case 483:
    case 481:
    case 470:
    case 469:
    case 465:
    case 449:
    case 446:
    case 445:
    case 443:
    case 441:
      extract_insn_cached (insn);
      if (register_operand (operands[0], VOIDmode))
        {
	  return 2;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 490:
    case 489:
    case 477:
    case 473:
    case 467:
    case 461:
    case 457:
    case 437:
    case 433:
      extract_insn_cached (insn);
      if (register_operand (operands[0], SImode))
        {
	  return 2;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 487:
    case 475:
    case 453:
    case 451:
    case 426:
    case 424:
      extract_insn_cached (insn);
      if (register_operand (operands[0], DImode))
        {
	  return 2;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 0)
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 0)
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }

    case 643:
    case 641:
    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 645:
    case 644:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
    case 555:
    case 554:
    case 551:
    case 548:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 105:
    case 104:
    case 95:
    case 90:
      return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);

    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 2 + get_attr_prefix_data16 (insn) + get_attr_length_address (insn);
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 1)
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 714:
    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else
        {
	  return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);
        }

    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      return 4;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      return 128 /* 0x80 */;

    case 1024:
    case 1023:
    case 528:
      return 3;

    case 854:
      return 135 /* 0x87 */;

    case 547:
    case 546:
    case 545:
      return 7;

    case 544:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 6;
        }
      else
        {
	  return 7;
        }

    case 542:
    case 537:
    case 531:
      return 12 /* 0xc */;

    case 541:
      return 13 /* 0xd */;

    case 540:
      return 11 /* 0xb */;

    case 538:
      return 14 /* 0xe */;

    case 535:
    case 534:
    case 530:
    case 527:
    case 30:
      return 1;

    case 526:
      return 0;

    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 543:
    case 536:
    case 533:
    case 532:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 75:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
    case 539:
    case 520:
      return 16 /* 0x10 */;

    case 673:
    case 672:
    case 515:
      return 5;

    case 504:
    case 503:
      return 6;

    case 478:
    case 462:
    case 458:
    case 438:
    case 434:
    case 160:
    case 159:
    case 29:
      return 2;

    default:
      return get_attr_modrm (insn) + get_attr_prefix_0f (insn) + 1 + get_attr_prefix_rep (insn) + get_attr_prefix_data16 (insn) + get_attr_length_immediate (insn) + get_attr_length_address (insn);

    }
}

extern int bypass_p PARAMS ((rtx));
int
bypass_p (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 79:
    case 78:
    case 41:
    case 40:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

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
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 0;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (const0_operand (operands[2], DImode))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (const0_operand (operands[2], SImode))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative == 0) || (which_alternative == 1))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 0;
        }

    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 0;
        }

    case 605:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 70 /* 0x46 */;
        }
      else
        {
	  return 0;
        }

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (which_alternative == 0))
        {
	  return 70 /* 0x46 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], TFmode))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 39 /* 0x27 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], XFmode))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 39 /* 0x27 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_type (insn) == TYPE_FOP) || ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 39 /* 0x27 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], DFmode))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 39 /* 0x27 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_type (insn) == TYPE_FOP) || ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 39 /* 0x27 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], SFmode))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 39 /* 0x27 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 1) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 555:
    case 554:
    case 551:
    case 548:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 3;
        }
      else
        {
	  return 0;
        }

    case 851:
    case 547:
    case 546:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 0;
        }

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 500:
    case 499:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (memory_operand (operands[0], VOIDmode)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (memory_operand (operands[0], VOIDmode))))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case 498:
    case 496:
    case 485:
    case 483:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 497:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 484:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 472:
    case 470:
    case 448:
    case 446:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 474:
    case 473:
    case 471:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 447:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 427:
    case 426:
    case 425:
    case 424:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 419:
    case 418:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 417:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 422:
    case 421:
    case 415:
    case 409:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 406:
    case 404:
    case 401:
    case 400:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (memory_operand (operands[1], VOIDmode)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 292:
    case 288:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 279:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 0) || (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((which_alternative != 0) && (which_alternative != 2)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 0) || (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_NONE)) || (((which_alternative != 0) && (which_alternative != 2)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 278:
    case 277:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) && (get_attr_memory (insn) == MEMORY_NONE)) || ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 276:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative != 1) && (which_alternative != 3)) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((which_alternative == 1) || (which_alternative == 3)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative != 1) && (which_alternative != 3)) && (get_attr_memory (insn) == MEMORY_NONE)) || (((which_alternative == 1) || (which_alternative == 3)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 11 /* 0xb */;
        }
      else
        {
	  return 0;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 215:
    case 202:
    case 201:
    case 196:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 348:
    case 347:
    case 346:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 318:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 296:
    case 294:
    case 293:
    case 291:
    case 290:
    case 289:
    case 287:
    case 244:
    case 243:
    case 241:
    case 240:
    case 239:
    case 238:
    case 237:
    case 236:
    case 235:
    case 234:
    case 233:
    case 232:
    case 229:
    case 228:
    case 227:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 185:
    case 184:
    case 181:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 3;
        }
      else
        {
	  return 0;
        }

    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 1)) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 1)) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && (which_alternative == 1))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 1)) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_PENTIUM))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))) || (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))) || (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))) || (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 112:
    case 109:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))) || (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))) || (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))) || (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_BOTH))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_LOAD))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode)))) && ((which_alternative == 1) && (memory_operand (operands[1], VOIDmode)))) || (((which_alternative != 1) || (memory_operand (operands[1], VOIDmode))) && ((which_alternative == 1) && (memory_operand (operands[1], VOIDmode))))))
        {
	  return 3;
        }
      else
        {
	  return 0;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode)))) && ((which_alternative == 0) && (memory_operand (operands[1], VOIDmode)))) || (((which_alternative != 0) || (memory_operand (operands[1], VOIDmode))) && ((which_alternative == 0) && (memory_operand (operands[1], VOIDmode))))))
        {
	  return 3;
        }
      else
        {
	  return 0;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 70:
    case 66:
    case 65:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_IMOV))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_IMOV)) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_IMOV)) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_IMOV)) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 74:
    case 73:
    case 72:
    case 61:
    case 55:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 50:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_IMOV))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
    case 86:
    case 60:
    case 54:
    case 53:
    case 47:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 545:
    case 544:
    case 85:
    case 68:
    case 52:
    case 46:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 814:
    case 813:
    case 770:
    case 769:
    case 645:
    case 644:
    case 638:
    case 636:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 84:
    case 67:
    case 51:
    case 45:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 407:
    case 405:
    case 403:
    case 402:
    case 399:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (memory_operand (operands[1], VOIDmode))) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (memory_operand (operands[1], VOIDmode)))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (memory_operand (operands[1], VOIDmode)))
        {
	  return 3;
        }
      else
        {
	  return 0;
        }

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 280:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 283:
    case 282:
    case 281:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 854:
    case 853:
    case 852:
    case 850:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 75:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      return 0;

    default:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    }
}

extern int insn_alts PARAMS ((rtx));
int
insn_alts (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (const0_operand (operands[2], DImode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (const0_operand (operands[2], SImode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative == 0) || (which_alternative == 1))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_memory (insn) == MEMORY_BOTH)) || ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], TFmode)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_type (insn) == TYPE_FDIV) || ((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD))) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], XFmode)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_type (insn) == TYPE_FDIV) || ((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD))) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 2) && (mult_operator (operands[3], DFmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_type (insn) == TYPE_FDIV) || ((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD))) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], DFmode)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_type (insn) == TYPE_FDIV) || ((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD))) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 2) && (mult_operator (operands[3], SFmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_type (insn) == TYPE_FDIV) || ((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD))) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], SFmode)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_type (insn) == TYPE_FDIV) || ((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD))) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 555:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (mult_operator (operands[3], TFmode))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], TFmode)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 554:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (mult_operator (operands[3], XFmode))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], XFmode)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode)))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (mult_operator (operands[3], SFmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (mult_operator (operands[3], SFmode))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], SFmode)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 547:
    case 546:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 32 /* 0x20 */;
        }
      else
        {
	  return 0;
        }

    case 545:
    case 544:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case 498:
    case 496:
    case 485:
    case 483:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 497:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 484:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 472:
    case 470:
    case 448:
    case 446:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 474:
    case 473:
    case 471:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 447:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 427:
    case 426:
    case 425:
    case 424:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 419:
    case 418:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 417:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 422:
    case 421:
    case 415:
    case 409:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 406:
    case 404:
    case 401:
    case 400:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((memory_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 292:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_LOAD)) || (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_NONE))) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 288:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 279:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) && (which_alternative != 2)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) && (which_alternative != 2)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 278:
    case 277:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 1) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 1) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 276:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) && (which_alternative != 3)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) || (which_alternative == 3)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) && (which_alternative != 3)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) || (which_alternative == 3)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 337:
    case 336:
    case 335:
    case 315:
    case 314:
    case 313:
    case 293:
    case 240:
    case 239:
    case 238:
    case 214:
    case 212:
    case 211:
    case 210:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 215:
    case 202:
    case 201:
    case 196:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 348:
    case 347:
    case 346:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 338:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 318:
    case 316:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 296:
    case 294:
    case 291:
    case 290:
    case 289:
    case 287:
    case 244:
    case 243:
    case 241:
    case 237:
    case 236:
    case 235:
    case 234:
    case 233:
    case 232:
    case 229:
    case 228:
    case 227:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 213:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 185:
    case 184:
    case 181:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 231:
    case 230:
    case 226:
    case 183:
    case 182:
    case 180:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 851:
    case 814:
    case 813:
    case 770:
    case 769:
    case 645:
    case 644:
    case 638:
    case 636:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
    case 500:
    case 499:
    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) && (((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || (((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 0) || (which_alternative == 1)) && (((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || ((((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && (which_alternative == 1)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_PENTIUM))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 112:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 109:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))) || (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))) || (((get_attr_memory (insn) == MEMORY_BOTH) || (get_attr_memory (insn) == MEMORY_LOAD)) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)) || ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (memory_operand (operands[1], VOIDmode))) && ((which_alternative == 1) && (memory_operand (operands[1], VOIDmode)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) || (memory_operand (operands[1], VOIDmode))) && ((which_alternative == 0) && (memory_operand (operands[1], VOIDmode)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 70:
    case 66:
    case 65:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_IMOV))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_IMOV)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_IMOV)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 74:
    case 73:
    case 72:
    case 61:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 405:
    case 108:
    case 56:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (memory_operand (operands[1], VOIDmode))) || ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 55:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 52:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 50:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_IMOV))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)) || (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 86:
    case 60:
    case 54:
    case 53:
    case 47:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 85:
    case 68:
    case 46:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 84:
    case 67:
    case 51:
    case 45:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 407:
    case 403:
    case 402:
    case 399:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 106:
    case 81:
    case 80:
    case 62:
    case 43:
    case 42:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (memory_operand (operands[1], VOIDmode)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 280:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 8:
    case 7:
    case 6:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 2;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)) || ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))) || ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 283:
    case 282:
    case 281:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 1;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 854:
    case 853:
    case 852:
    case 850:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 75:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      return 0;

    default:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_memory (insn) == MEMORY_BOTH) || ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    }
}

extern int internal_dfa_insn_code PARAMS ((rtx));
int
internal_dfa_insn_code (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 851:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 23 /* 0x17 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (const0_operand (operands[2], DImode))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (const0_operand (operands[2], SImode))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 814:
    case 813:
    case 770:
    case 769:
    case 645:
    case 644:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative == 0) || (which_alternative == 1))))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 638:
    case 636:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 25 /* 0x19 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 1;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 605:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 3;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 15 /* 0xf */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (which_alternative == 0))
        {
	  return 15 /* 0xf */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], TFmode)))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 14 /* 0xe */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], XFmode)))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 14 /* 0xe */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 2) && (mult_operator (operands[3], DFmode))))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 14 /* 0xe */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], DFmode)))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 14 /* 0xe */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 2) && (mult_operator (operands[3], SFmode))))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 14 /* 0xe */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FOP))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], SFmode)))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 14 /* 0xe */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 555:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (mult_operator (operands[3], TFmode))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], TFmode)))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 554:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (mult_operator (operands[3], XFmode))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], XFmode)))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode)))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (mult_operator (operands[3], SFmode))))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (mult_operator (operands[3], SFmode))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (mult_operator (operands[3], SFmode)))
        {
	  return 13 /* 0xd */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 547:
    case 546:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 20 /* 0x14 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 545:
    case 544:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 7;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 11 /* 0xb */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 500:
    case 499:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (memory_operand (operands[0], VOIDmode)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (memory_operand (operands[0], VOIDmode))))
        {
	  return 23 /* 0x17 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 498:
    case 496:
    case 485:
    case 483:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 497:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 484:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_1_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 472:
    case 470:
    case 448:
    case 446:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[1], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[1], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 474:
    case 473:
    case 471:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 447:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 427:
    case 426:
    case 425:
    case 424:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (const_int_operand (operands[2], VOIDmode))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 1) || (! (const_int_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 419:
    case 418:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 417:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 422:
    case 421:
    case 415:
    case 409:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_ALU)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 406:
    case 404:
    case 401:
    case 400:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (memory_operand (operands[1], VOIDmode)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 292:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 288:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || (which_alternative == 1))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && (which_alternative != 1))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 279:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) && (which_alternative != 2)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) && (which_alternative != 2)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 278:
    case 277:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 1) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative != 1) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 276:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) && (which_alternative != 3)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) || (which_alternative == 3)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) && (which_alternative != 3)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 1) || (which_alternative == 3)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 2;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 0;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 337:
    case 336:
    case 335:
    case 315:
    case 314:
    case 313:
    case 293:
    case 240:
    case 239:
    case 238:
    case 214:
    case 212:
    case 211:
    case 210:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (which_alternative == 2)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 215:
    case 202:
    case 201:
    case 196:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 24 /* 0x18 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 348:
    case 347:
    case 346:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 338:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 318:
    case 316:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 296:
    case 294:
    case 291:
    case 290:
    case 289:
    case 287:
    case 244:
    case 243:
    case 241:
    case 237:
    case 236:
    case 235:
    case 234:
    case 233:
    case 232:
    case 229:
    case 228:
    case 227:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 213:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 185:
    case 184:
    case 181:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 231:
    case 230:
    case 226:
    case 183:
    case 182:
    case 180:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 25 /* 0x19 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 1)) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 1)) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && (which_alternative == 1))))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || (which_alternative == 1)) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_PENTIUM))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 112:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 109:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE))))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_NONE) || (get_attr_memory (insn) == MEMORY_LOAD))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((immediate_operand (operands[1], VOIDmode)) || (get_attr_memory (insn) == MEMORY_STORE))))
        {
	  return 6;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (((which_alternative == 3) || (which_alternative == 4)) && ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 8;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 1) || (memory_operand (operands[1], VOIDmode))) && ((which_alternative == 1) && (memory_operand (operands[1], VOIDmode)))))
        {
	  return 19 /* 0x13 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode)))))
        {
	  return 8;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 0) || (memory_operand (operands[1], VOIDmode))) && ((which_alternative == 0) && (memory_operand (operands[1], VOIDmode)))))
        {
	  return 19 /* 0x13 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 70:
    case 66:
    case 65:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_IMOV))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_IMOV)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_type (insn) == TYPE_IMOV)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 74:
    case 73:
    case 72:
    case 61:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 7;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))) && (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 405:
    case 108:
    case 56:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 55:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 7;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 52:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 7;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 50:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_type (insn) == TYPE_IMOV))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_PU) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (((get_attr_imm_disp (insn) == IMM_DISP_TRUE) || (! (get_attr_type (insn) == TYPE_IMOV))) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 86:
    case 60:
    case 54:
    case 53:
    case 47:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 7;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 85:
    case 68:
    case 46:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 7;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 84:
    case 67:
    case 51:
    case 45:
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 7;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_UV) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_pent_pair (insn) == PENT_PAIR_NP) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 407:
    case 403:
    case 402:
    case 399:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 106:
    case 81:
    case 80:
    case 62:
    case 43:
    case 42:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if (((ix86_cpu) == (CPU_PENTIUM)))
        {
	  return 9;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return 8;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (memory_operand (operands[1], VOIDmode)))
        {
	  return 19 /* 0x13 */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 280:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 8:
    case 7:
    case 6:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 25 /* 0x19 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case 283:
    case 282:
    case 281:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((! (get_attr_imm_disp (insn) == IMM_DISP_TRUE)) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && ((get_attr_imm_disp (insn) == IMM_DISP_TRUE) && (get_attr_memory (insn) == MEMORY_NONE)))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 854:
    case 853:
    case 852:
    case 850:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 75:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      return 29 /* 0x1d */;

    default:
      if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUM))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return 27 /* 0x1b */;
        }
      else
        {
	  return 29 /* 0x1d */;
        }

    }
}

extern int result_ready_cost PARAMS ((rtx));
int
result_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (! (constant_call_address_operand (operands[1], VOIDmode))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 1) && (! (const0_operand (operands[2], DImode)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 1) && (! (const0_operand (operands[2], SImode)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 645:
    case 644:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 7;
        }
      else if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_ATHLON))) && ((which_alternative == 0) || (which_alternative == 1)))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((which_alternative == 0) || (which_alternative == 1)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_ATHLON))) && ((which_alternative == 0) || (which_alternative == 1)))
        {
	  return 7;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((which_alternative == 0) || (which_alternative == 1)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 15 /* 0xf */;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 1;
        }

    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 15 /* 0xf */;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 10 /* 0xa */;
        }
      else if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 100 /* 0x64 */;
        }
      else if ((((ix86_cpu) == (CPU_K6))) || (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 56 /* 0x38 */;
        }
      else
        {
	  return 1;
        }

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_ATHLON))) && (which_alternative == 0))
        {
	  return 100 /* 0x64 */;
        }
      else if (((((ix86_cpu) == (CPU_K6))) && (which_alternative == 0)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (which_alternative == 0)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (((((ix86_cpu) == (CPU_K6))) && (get_attr_type (insn) == TYPE_FDIV)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (mult_operator (operands[3], TFmode)))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], TFmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (get_attr_type (insn) == TYPE_FOP)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], TFmode))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (((((ix86_cpu) == (CPU_K6))) && (get_attr_type (insn) == TYPE_FDIV)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (mult_operator (operands[3], XFmode)))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], XFmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (get_attr_type (insn) == TYPE_FOP)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], XFmode))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if (((((ix86_cpu) == (CPU_K6))) && (get_attr_type (insn) == TYPE_FDIV)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((which_alternative != 2) && (mult_operator (operands[3], DFmode))))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && ((get_attr_type (insn) == TYPE_FOP) || ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (get_attr_type (insn) == TYPE_FOP)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_FOP) || ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (((((ix86_cpu) == (CPU_K6))) && (get_attr_type (insn) == TYPE_FDIV)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (mult_operator (operands[3], DFmode)))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], DFmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (get_attr_type (insn) == TYPE_FOP)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], DFmode))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if (((((ix86_cpu) == (CPU_K6))) && (get_attr_type (insn) == TYPE_FDIV)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((which_alternative != 2) && (mult_operator (operands[3], SFmode))))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && ((get_attr_type (insn) == TYPE_FOP) || ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (get_attr_type (insn) == TYPE_FOP)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_FOP) || ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (((((ix86_cpu) == (CPU_K6))) && (get_attr_type (insn) == TYPE_FDIV)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && (get_attr_type (insn) == TYPE_FDIV))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (mult_operator (operands[3], SFmode)))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], SFmode))))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (get_attr_type (insn) == TYPE_FOP)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_FOP) || (mult_operator (operands[3], SFmode))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 555:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (mult_operator (operands[3], TFmode)))
        {
	  return 5;
        }
      else if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (! (mult_operator (operands[3], TFmode)))))
        {
	  return 3;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 554:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (mult_operator (operands[3], XFmode)))
        {
	  return 5;
        }
      else if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (! (mult_operator (operands[3], XFmode)))))
        {
	  return 3;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((which_alternative == 0) && (mult_operator (operands[3], SFmode))))
        {
	  return 5;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && (which_alternative == 0))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (mult_operator (operands[3], SFmode)))
        {
	  return 5;
        }
      else if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 4;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)) || (! (mult_operator (operands[3], SFmode)))))
        {
	  return 3;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (! (constant_call_address_operand (operands[0], VOIDmode))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (memory_operand (operands[0], VOIDmode)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 417:
    case 414:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && (which_alternative == 1))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 3;
        }
      else if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 42 /* 0x2a */;
        }
      else if ((((ix86_cpu) == (CPU_K6))) || (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 1;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 5;
        }
      else if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 4;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && (which_alternative == 3))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 420:
    case 209:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && (which_alternative == 2))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_ATHLON))) && ((which_alternative == 0) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 10 /* 0xa */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if (((((ix86_cpu) == (CPU_ATHLON))) && (which_alternative == 0)) || ((((ix86_cpu) == (CPU_K6))) && (which_alternative == 0)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 4;
        }
      else if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 3;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) || (((ix86_cpu) == (CPU_K6))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if (((((ix86_cpu) == (CPU_ATHLON))) && (which_alternative != 0)) || ((which_alternative == 1) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if (((((ix86_cpu) == (CPU_ATHLON))) && (which_alternative == 0)) || ((((ix86_cpu) == (CPU_K6))) && (which_alternative == 0)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_ATHLON))) && ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 10 /* 0xa */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_ATHLON))) || (((ix86_cpu) == (CPU_K6))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_ATHLON))) && (((which_alternative == 0) || (which_alternative == 1)) && ((get_attr_memory (insn) == MEMORY_LOAD) && (which_alternative == 1))))
        {
	  return 10 /* 0xa */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if (((((ix86_cpu) == (CPU_ATHLON))) && ((which_alternative == 0) || (which_alternative == 1))) || ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 0) || (which_alternative == 1))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_ATHLON))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 10 /* 0xa */;
        }
      else if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if (((((ix86_cpu) == (CPU_ATHLON))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) || ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if (((((ix86_cpu) == (CPU_ATHLON))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) || ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((which_alternative == 1) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((which_alternative == 0) && (memory_operand (operands[1], VOIDmode))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 851:
    case 547:
    case 546:
    case 545:
    case 544:
    case 500:
    case 499:
    case 85:
    case 79:
    case 78:
    case 68:
    case 52:
    case 46:
    case 41:
    case 40:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 77:
    case 62:
    case 58:
    case 57:
    case 56:
    case 49:
    case 48:
    case 43:
    case 42:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (memory_operand (operands[1], VOIDmode)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (((((ix86_cpu) == (CPU_ATHLON))) && (which_alternative == 0)) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 3;
        }
      else if ((((ix86_cpu) == (CPU_K6))) && (which_alternative == 0))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if ((((ix86_cpu) == (CPU_ATHLON))) || ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_memory (insn) == MEMORY_LOAD)))
        {
	  return 3;
        }
      else if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 36:
    case 33:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 854:
    case 853:
    case 852:
    case 850:
    case 814:
    case 813:
    case 770:
    case 769:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 638:
    case 636:
    case 605:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 84:
    case 75:
    case 67:
    case 51:
    case 45:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      return 1;

    default:
      if ((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    }
}

extern int athlon_load_unit_ready_cost PARAMS ((rtx));
int
athlon_load_unit_ready_cost (insn)
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

extern int athlon_fp_store_unit_ready_cost PARAMS ((rtx));
int
athlon_fp_store_unit_ready_cost (insn)
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

extern int athlon_fp_muladd_unit_ready_cost PARAMS ((rtx));
int
athlon_fp_muladd_unit_ready_cost (insn)
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

extern int athlon_fp_add_unit_ready_cost PARAMS ((rtx));
int
athlon_fp_add_unit_ready_cost (insn)
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

extern int athlon_fp_mul_unit_ready_cost PARAMS ((rtx));
int
athlon_fp_mul_unit_ready_cost (insn)
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

extern int athlon_fp_unit_ready_cost PARAMS ((rtx));
int
athlon_fp_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 645:
    case 644:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 7;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 7;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 7;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FDIV) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 24 /* 0x18 */;
        }
      else if (((mult_operator (operands[3], TFmode)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((get_attr_type (insn) == TYPE_FOP) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FDIV) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 24 /* 0x18 */;
        }
      else if (((mult_operator (operands[3], XFmode)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((get_attr_type (insn) == TYPE_FOP) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FDIV) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((which_alternative != 2) && (mult_operator (operands[3], DFmode))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((get_attr_type (insn) == TYPE_FOP) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FDIV) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 24 /* 0x18 */;
        }
      else if (((mult_operator (operands[3], DFmode)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((get_attr_type (insn) == TYPE_FOP) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FDIV) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 24 /* 0x18 */;
        }
      else if ((((which_alternative != 2) && (mult_operator (operands[3], SFmode))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((get_attr_type (insn) == TYPE_FOP) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FDIV) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 24 /* 0x18 */;
        }
      else if (((mult_operator (operands[3], SFmode)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((get_attr_type (insn) == TYPE_FOP) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 555:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], TFmode)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((! (mult_operator (operands[3], TFmode))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 554:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], XFmode)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((! (mult_operator (operands[3], XFmode))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) && (mult_operator (operands[3], SFmode))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || (((which_alternative == 0) && (! (mult_operator (operands[3], SFmode)))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], SFmode)) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((! (mult_operator (operands[3], SFmode))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && ((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 2;
        }
      else if ((which_alternative == 0) && (((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE))) && ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 4;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
      if (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))) && ((get_attr_memory (insn) == MEMORY_LOAD) && ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 10 /* 0xa */;
        }
      else if (((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE))) && ((((get_attr_memory (insn) == MEMORY_LOAD) && ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE)))) && ((((get_attr_memory (insn) == MEMORY_LOAD) && ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && ((((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && (which_alternative == 1)) && ((get_attr_memory (insn) == MEMORY_LOAD) && ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))))))
        {
	  return 10 /* 0xa */;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && (which_alternative == 1)) && ((((get_attr_memory (insn) == MEMORY_LOAD) && ((which_alternative != 1) && (((ix86_cpu) == (CPU_ATHLON))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative != 1) && (((ix86_cpu) == (CPU_ATHLON))))))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((which_alternative != 0) && (which_alternative != 1)) || (((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE))) || (which_alternative != 1))) && ((((get_attr_memory (insn) == MEMORY_LOAD) && ((which_alternative != 1) && (((ix86_cpu) == (CPU_ATHLON))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && ((which_alternative != 1) && (((ix86_cpu) == (CPU_ATHLON))))))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((which_alternative == 3) || ((which_alternative == 4) || ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))) && ((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 10 /* 0xa */;
        }
      else if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((which_alternative == 3) || ((which_alternative == 4) || ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))) && ((((get_attr_memory (insn) == MEMORY_LOAD) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((ix86_cpu) == (CPU_ATHLON))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((ix86_cpu) == (CPU_ATHLON))))))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((which_alternative != 3) && ((which_alternative != 4) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE)))))) && ((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 10 /* 0xa */;
        }
      else if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((which_alternative != 3) && ((which_alternative != 4) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE)))))) && ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 2;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 3;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if (((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 3;
        }
      else
        {
	  return 100 /* 0x64 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 100 /* 0x64 */;

    }
}

extern int athlon_muldiv_unit_ready_cost PARAMS ((rtx));
int
athlon_muldiv_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 5;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 42 /* 0x2a */;

    }
}

extern unsigned int athlon_muldiv_unit_blockage_range PARAMS ((rtx));
unsigned int
athlon_muldiv_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65578 /* min 1, max 42 */;

    }
}

extern int athlon_ieu_unit_ready_cost PARAMS ((rtx));
int
athlon_ieu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 1) && (! (const0_operand (operands[2], DImode)))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative == 1) && (const0_operand (operands[2], DImode))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 1) && (! (const0_operand (operands[2], SImode)))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative == 1) && (const0_operand (operands[2], SImode))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || (which_alternative == 3)) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 15 /* 0xf */;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))) || ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_ISHIFT) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 2) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_ALU) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if ((((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 0) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if (((get_attr_type (insn) == TYPE_ISHIFT) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_ALU) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_ISHIFT) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_ALU) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 292:
    case 288:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 0) || (which_alternative == 1)) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 5;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 223:
    case 222:
    case 221:
    case 219:
    case 218:
    case 217:
    case 216:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], QImode)) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (incdec_operand (operands[2], QImode))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 3) && (incdec_operand (operands[2], QImode))) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 3) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 220:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], HImode)) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (incdec_operand (operands[2], HImode))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && (incdec_operand (operands[2], HImode))) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative == 2) && (((ix86_cpu) == (CPU_ATHLON)))) || (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], SImode)) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (incdec_operand (operands[2], SImode))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_INCDEC) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_ALU) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_INCDEC) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_ALU) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 200:
    case 199:
    case 198:
    case 197:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], DImode)) && (((ix86_cpu) == (CPU_ATHLON)))) || ((! (incdec_operand (operands[2], DImode))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_INCDEC) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_ALU) && (((ix86_cpu) == (CPU_ATHLON))))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))) && (((ix86_cpu) == (CPU_ATHLON)))) || (((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 70:
    case 66:
    case 65:
      if (((get_attr_type (insn) == TYPE_IMOVX) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_IMOV) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if ((((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((get_attr_type (insn) == TYPE_IMOV) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))) && (((ix86_cpu) == (CPU_ATHLON)))) || ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
    case 640:
    case 639:
    case 638:
    case 637:
    case 636:
    case 605:
    case 547:
    case 546:
    case 545:
    case 544:
    case 529:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 294:
    case 293:
    case 291:
    case 290:
    case 289:
    case 287:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 242:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 111:
    case 110:
    case 108:
    case 107:
    case 106:
    case 86:
    case 85:
    case 84:
    case 81:
    case 80:
    case 79:
    case 78:
    case 77:
    case 74:
    case 73:
    case 72:
    case 69:
    case 68:
    case 67:
    case 64:
    case 63:
    case 62:
    case 61:
    case 60:
    case 58:
    case 57:
    case 56:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 43:
    case 42:
    case 41:
    case 40:
    case 39:
    case 38:
    case 37:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case 115:
    case 112:
    case 109:
    case 9:
    case 6:
    case 3:
    case 0:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON)))) || ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON)))))
        {
	  return 1;
        }
      else
        {
	  return 42 /* 0x2a */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 42 /* 0x2a */;

    }
}

extern unsigned int athlon_ieu_unit_blockage_range PARAMS ((rtx));
unsigned int
athlon_ieu_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65551 /* min 1, max 15 */;

    }
}

extern int athlon_directdec_unit_ready_cost PARAMS ((rtx));
int
athlon_directdec_unit_ready_cost (insn)
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

extern int athlon_vectordec_unit_ready_cost PARAMS ((rtx));
int
athlon_vectordec_unit_ready_cost (insn)
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

extern unsigned int athlon_vectordec_unit_blockage_range PARAMS ((rtx));
unsigned int
athlon_vectordec_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 926:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE)))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 1) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 3))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 1) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE)))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) || (((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE))) || (which_alternative != 1))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && ((which_alternative != 4) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE)))))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode)))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 4) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 714:
    case 643:
    case 641:
    case 82:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 1)) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode)))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if ((! (memory_operand (operands[1], VOIDmode))) && (((ix86_cpu) == (CPU_ATHLON))))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 924:
    case 854:
    case 791:
    case 789:
    case 787:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 645:
    case 644:
    case 642:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 412:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 86:
    case 79:
    case 78:
    case 75:
    case 47:
    case 41:
    case 40:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      return 65537 /* min 1, max 1 */;

    default:
      if (((ix86_cpu) == (CPU_ATHLON)))
        {
	  return 1 /* min 0, max 1 */;
        }
      else
        {
	  return 65537 /* min 1, max 1 */;
        }

    }
}

extern int k6_fpu_unit_ready_cost PARAMS ((rtx));
int
k6_fpu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], TFmode)) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], XFmode)) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && (mult_operator (operands[3], DFmode))) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], DFmode)) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], SFmode)) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 555:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], TFmode)) && (((ix86_cpu) == (CPU_K6)))) || ((! (mult_operator (operands[3], TFmode))) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 554:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], XFmode)) && (((ix86_cpu) == (CPU_K6)))) || ((! (mult_operator (operands[3], XFmode))) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) && (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_K6)))) || (((which_alternative == 0) && (! (mult_operator (operands[3], SFmode)))) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (((mult_operator (operands[3], SFmode)) && (((ix86_cpu) == (CPU_K6)))) || ((! (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 56 /* 0x38 */;

    }
}

extern unsigned int k6_fpu_unit_blockage_range PARAMS ((rtx));
unsigned int
k6_fpu_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 131128 /* min 2, max 56 */;

    }
}

extern int k6_store_unit_ready_cost PARAMS ((rtx));
int
k6_store_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (! (const0_operand (operands[2], DImode)))) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) || (const0_operand (operands[2], DImode))) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (! (const0_operand (operands[2], SImode)))) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) || (const0_operand (operands[2], SImode))) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 417:
    case 414:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if ((which_alternative != 3) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 420:
    case 209:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if ((which_alternative != 2) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) && (! (pic_symbolic_operand (operands[2], SImode)))) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if (((which_alternative != 2) && (! (pic_symbolic_operand (operands[2], SImode)))) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if (((which_alternative != 2) && (! (pic_symbolic_operand (operands[2], DImode)))) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((memory_operand (operands[1], VOIDmode)) && (((ix86_cpu) == (CPU_K6)))) || ((! (memory_operand (operands[1], VOIDmode))) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if ((((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 4) || ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((memory_operand (operands[1], VOIDmode)) && (((ix86_cpu) == (CPU_K6)))) || ((! (memory_operand (operands[1], VOIDmode))) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 854:
    case 852:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 84:
    case 67:
    case 51:
    case 45:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))) && ((((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))) || ((! (get_attr_memory (insn) == MEMORY_BOTH)) && ((! (get_attr_memory (insn) == MEMORY_STORE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 2;
        }
      else if ((((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))) && (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6))))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      extract_insn_cached (insn);
      if (((memory_operand (operands[1], VOIDmode)) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 500:
    case 499:
    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (((memory_operand (operands[1], VOIDmode)) && (((ix86_cpu) == (CPU_K6)))) || ((! (memory_operand (operands[1], VOIDmode))) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 953:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 860:
    case 859:
    case 858:
    case 853:
    case 851:
    case 850:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 814:
    case 813:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 770:
    case 769:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 645:
    case 644:
    case 642:
    case 638:
    case 636:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
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
    case 530:
    case 529:
    case 528:
    case 527:
    case 526:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 504:
    case 503:
    case 502:
    case 501:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 85:
    case 75:
    case 68:
    case 52:
    case 46:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 30:
    case 29:
    case 28:
    case 27:
    case 26:
    case 25:
    case 24:
    case 23:
    case 22:
    case 21:
    case 20:
    case 19:
    case 18:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      return 10 /* 0xa */;

    default:
      if (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_STORE) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    }
}

extern unsigned int k6_store_unit_blockage_range PARAMS ((rtx));
unsigned int
k6_store_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65546 /* min 1, max 10 */;

    }
}

extern int k6_load_unit_ready_cost PARAMS ((rtx));
int
k6_load_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if ((! (constant_call_address_operand (operands[1], VOIDmode))) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if ((! (constant_call_address_operand (operands[0], VOIDmode))) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (memory_operand (operands[1], VOIDmode))) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (memory_operand (operands[1], VOIDmode))) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 851:
    case 547:
    case 546:
    case 545:
    case 544:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 85:
    case 68:
    case 52:
    case 46:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 500:
    case 499:
    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if (((memory_operand (operands[0], VOIDmode)) && (((ix86_cpu) == (CPU_K6)))) || ((! (memory_operand (operands[0], VOIDmode))) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 77:
    case 62:
    case 58:
    case 57:
    case 56:
    case 49:
    case 48:
    case 43:
    case 42:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if ((memory_operand (operands[1], VOIDmode)) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if ((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 854:
    case 853:
    case 852:
    case 850:
    case 814:
    case 813:
    case 770:
    case 769:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 645:
    case 644:
    case 642:
    case 638:
    case 636:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 84:
    case 75:
    case 67:
    case 51:
    case 45:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      return 10 /* 0xa */;

    default:
      if (((get_attr_memory (insn) == MEMORY_BOTH) && (((ix86_cpu) == (CPU_K6)))) || ((get_attr_memory (insn) == MEMORY_LOAD) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 1;
        }
      else
        {
	  return 10 /* 0xa */;
        }

    }
}

extern unsigned int k6_load_unit_blockage_range PARAMS ((rtx));
unsigned int
k6_load_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65546 /* min 1, max 10 */;

    }
}

extern int k6_branch_unit_ready_cost PARAMS ((rtx));
int
k6_branch_unit_ready_cost (insn)
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

extern int k6_alu_unit_ready_cost PARAMS ((rtx));
int
k6_alu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 1) && (const0_operand (operands[2], DImode))) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || ((((which_alternative == 1) && (! (const0_operand (operands[2], DImode)))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 1) && (const0_operand (operands[2], SImode))) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || ((((which_alternative == 1) && (! (const0_operand (operands[2], SImode)))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((get_attr_type (insn) == TYPE_ALU) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((get_attr_type (insn) == TYPE_ISHIFT) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if (((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((((which_alternative == 0) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if (((get_attr_type (insn) == TYPE_ALU) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((get_attr_type (insn) == TYPE_ISHIFT) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((get_attr_type (insn) == TYPE_ALU) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((get_attr_type (insn) == TYPE_ISHIFT) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 292:
    case 288:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((which_alternative == 0) || (which_alternative == 1)) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 3) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 2;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 223:
    case 222:
    case 221:
    case 219:
    case 218:
    case 217:
    case 216:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], QImode)) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((! (incdec_operand (operands[2], QImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 3) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((((which_alternative != 3) && (incdec_operand (operands[2], QImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 220:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], HImode)) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((! (incdec_operand (operands[2], HImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((((which_alternative != 2) && (incdec_operand (operands[2], HImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], SImode)) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((! (incdec_operand (operands[2], SImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((get_attr_type (insn) == TYPE_INCDEC) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((get_attr_type (insn) == TYPE_ALU) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((get_attr_type (insn) == TYPE_INCDEC) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((get_attr_type (insn) == TYPE_ALU) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 200:
    case 199:
    case 198:
    case 197:
      extract_insn_cached (insn);
      if (((incdec_operand (operands[2], DImode)) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((! (incdec_operand (operands[2], DImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || (((get_attr_type (insn) == TYPE_INCDEC) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((get_attr_type (insn) == TYPE_ALU) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6))))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 638:
    case 636:
    case 547:
    case 546:
    case 500:
    case 499:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || ((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || (((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 70:
    case 66:
    case 65:
      if (((get_attr_type (insn) == TYPE_IMOV) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || ((get_attr_type (insn) == TYPE_IMOVX) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || ((((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_IMOV) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || (((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 86:
    case 74:
    case 73:
    case 72:
    case 61:
    case 60:
    case 55:
    case 54:
    case 53:
    case 47:
      if ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))) && ((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6))))) || ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 294:
    case 293:
    case 291:
    case 290:
    case 289:
    case 287:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 242:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 111:
    case 110:
    case 108:
    case 107:
    case 106:
    case 81:
    case 80:
    case 69:
    case 64:
    case 63:
    case 62:
    case 56:
    case 43:
    case 42:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 432:
    case 431:
    case 423:
    case 112:
    case 109:
    case 9:
    case 6:
    case 3:
    case 0:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))) || ((which_alternative == 1) && (((get_attr_memory (insn) == MEMORY_NONE) && (((ix86_cpu) == (CPU_K6)))) || ((! (get_attr_memory (insn) == MEMORY_NONE)) && (((ix86_cpu) == (CPU_K6)))))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 17 /* 0x11 */;

    }
}

extern unsigned int k6_alu_unit_blockage_range PARAMS ((rtx));
unsigned int
k6_alu_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65553 /* min 1, max 17 */;

    }
}

extern int k6_alux_unit_ready_cost PARAMS ((rtx));
int
k6_alux_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 416:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && (((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))) || ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) && (general_operand (operands[0], QImode)))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && (((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))) || (((which_alternative == 0) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (general_operand (operands[0], QImode)))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 422:
    case 421:
    case 420:
    case 419:
    case 418:
    case 417:
    case 415:
    case 413:
    case 409:
    case 408:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_ISHIFT) || ((get_attr_type (insn) == TYPE_ALU) && (general_operand (operands[0], QImode)))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 2;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 217:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((! (incdec_operand (operands[2], QImode))) || (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative != 3) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative != 2) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 202:
    case 201:
    case 196:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && (((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_INCDEC)) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 659:
    case 658:
    case 115:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative == 0) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 432:
    case 431:
    case 423:
    case 112:
    case 109:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((which_alternative != 0) || (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && (((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 70:
    case 66:
    case 65:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((get_attr_type (insn) == TYPE_IMOVX) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && ((((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && (((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) && (general_operand (operands[0], QImode))))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 605:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      if (((ix86_cpu) == (CPU_K6)))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 638:
    case 636:
    case 547:
    case 546:
    case 348:
    case 347:
    case 346:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 318:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 296:
    case 294:
    case 293:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 110:
    case 107:
    case 69:
    case 64:
    case 63:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      extract_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) && (general_operand (operands[0], QImode)))
        {
	  return 1;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 17 /* 0x11 */;

    }
}

extern unsigned int k6_alux_unit_blockage_range PARAMS ((rtx));
unsigned int
k6_alux_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65553 /* min 1, max 17 */;

    }
}

extern int fpu_unit_ready_cost PARAMS ((rtx));
int
fpu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], TFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], XFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (mult_operator (operands[3], DFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], DFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], SFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 555:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], TFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((! (mult_operator (operands[3], TFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 554:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], XFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((! (mult_operator (operands[3], XFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if (((which_alternative == 0) && (! (mult_operator (operands[3], SFmode)))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], SFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((! (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 4;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 643:
    case 642:
    case 641:
    case 127:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 645:
    case 644:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 56 /* 0x38 */;

    }
}

extern unsigned int fpu_unit_blockage_range PARAMS ((rtx));
unsigned int
fpu_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65592 /* min 1, max 56 */;

    }
}

extern int ppro_p34_unit_ready_cost PARAMS ((rtx));
int
ppro_p34_unit_ready_cost (insn)
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

extern int ppro_p2_unit_ready_cost PARAMS ((rtx));
int
ppro_p2_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 3;

    }
}

extern int ppro_p01_unit_ready_cost PARAMS ((rtx));
int
ppro_p01_unit_ready_cost (insn)
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

extern int ppro_p0_unit_ready_cost PARAMS ((rtx));
int
ppro_p0_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (! (const0_operand (operands[2], DImode)))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (! (const0_operand (operands[2], SImode)))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 645:
    case 644:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 643:
    case 642:
    case 641:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 2;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], TFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], XFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (mult_operator (operands[3], DFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], DFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], SFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_FOP) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 555:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], TFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((! (mult_operator (operands[3], TFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 554:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], XFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((! (mult_operator (operands[3], XFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if (((which_alternative == 0) && (! (mult_operator (operands[3], SFmode)))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if ((mult_operator (operands[3], SFmode)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 5;
        }
      else if ((! (mult_operator (operands[3], SFmode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) && (((ix86_cpu) == (CPU_PENTIUMPRO)))) || ((get_attr_type (insn) == TYPE_ISHIFT) && (((ix86_cpu) == (CPU_PENTIUMPRO)))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if (((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (((ix86_cpu) == (CPU_PENTIUMPRO)))) || (((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))) && (((ix86_cpu) == (CPU_PENTIUMPRO)))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if ((get_attr_type (insn) == TYPE_ISHIFT) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (((ix86_cpu) == (CPU_PENTIUMPRO)))) || ((get_attr_type (insn) == TYPE_ISHIFT) && (((ix86_cpu) == (CPU_PENTIUMPRO)))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 4;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 3;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 432:
    case 431:
    case 423:
    case 135:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((ix86_cpu) == (CPU_PENTIUMPRO))))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case 605:
    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      if (((ix86_cpu) == (CPU_PENTIUMPRO)))
        {
	  return 1;
        }
      else
        {
	  return 56 /* 0x38 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 56 /* 0x38 */;

    }
}

extern unsigned int ppro_p0_unit_blockage_range PARAMS ((rtx));
unsigned int
ppro_p0_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65553 /* min 1, max 17 */;

    }
}

extern int function_units_used PARAMS ((rtx));
int
function_units_used (insn)
     rtx insn;
{
  enum attr_athlon_fpunits attr_athlon_fpunits = get_attr_athlon_fpunits (insn);
  enum attr_athlon_decode attr_athlon_decode = get_attr_athlon_decode (insn);
  enum attr_memory attr_memory = get_attr_memory (insn);
  enum attr_mode attr_mode = get_attr_mode (insn);
  enum attr_type attr_type = get_attr_type (insn);
  unsigned long accum = 0;

  accum |= (((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((((((((((attr_type == TYPE_ISHIFT) || ((attr_type == TYPE_ROTATE) || ((attr_type == TYPE_ISHIFT1) || ((attr_type == TYPE_ROTATE1) || ((attr_type == TYPE_LEA) || ((attr_type == TYPE_IBR) || (attr_type == TYPE_CLD))))))) || (attr_type == TYPE_IMUL)) || (attr_type == TYPE_IDIV)) || ((attr_type == TYPE_FOP) || ((attr_type == TYPE_FSGN) || (attr_type == TYPE_FISTP)))) || (attr_type == TYPE_FCMOV)) || (attr_type == TYPE_FCMP)) || (attr_type == TYPE_FMOV)) || (attr_type == TYPE_FMUL)) || ((attr_type == TYPE_FDIV) || (attr_type == TYPE_FPSPC)))) ? (1) : (0));
  accum |= ((((((ix86_cpu) == (CPU_PENTIUMPRO))) && (! ((attr_type == TYPE_IMOV) || (attr_type == TYPE_FMOV)))) || (((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((attr_type == TYPE_IMOV) || (attr_type == TYPE_FMOV))) && (attr_memory == MEMORY_NONE))) ? (2) : (0));
  accum |= (((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((attr_type == TYPE_POP) || ((attr_memory == MEMORY_LOAD) || (attr_memory == MEMORY_BOTH)))) ? (4) : (0));
  accum |= (((((ix86_cpu) == (CPU_PENTIUMPRO))) && ((attr_type == TYPE_PUSH) || ((attr_memory == MEMORY_STORE) || (attr_memory == MEMORY_BOTH)))) ? (8) : (0));
  accum |= (((((ix86_cpu) == (CPU_PENTIUMPRO))) && (((((attr_type == TYPE_FOP) || ((attr_type == TYPE_FSGN) || ((attr_type == TYPE_FMOV) || ((attr_type == TYPE_FCMP) || ((attr_type == TYPE_FCMOV) || (attr_type == TYPE_FISTP)))))) || (attr_type == TYPE_FMUL)) || ((attr_type == TYPE_FDIV) || (attr_type == TYPE_FPSPC))) || (attr_type == TYPE_IMUL))) ? (16) : (0));
  accum |= (((((ix86_cpu) == (CPU_K6))) && (((((attr_type == TYPE_ISHIFT) || ((attr_type == TYPE_ISHIFT1) || ((attr_type == TYPE_ROTATE) || ((attr_type == TYPE_ROTATE1) || ((attr_type == TYPE_ALU1) || ((attr_type == TYPE_NEGNOT) || (attr_type == TYPE_CLD))))))) || (((attr_type == TYPE_ALU) || ((attr_type == TYPE_ALU1) || ((attr_type == TYPE_NEGNOT) || ((attr_type == TYPE_ICMP) || ((attr_type == TYPE_TEST) || ((attr_type == TYPE_IMOVX) || (attr_type == TYPE_INCDEC))))))) && (general_operand (operands[0], QImode)))) || (attr_type == TYPE_IMUL)) || (attr_type == TYPE_IDIV))) ? (32) : (0));
  accum |= (((((ix86_cpu) == (CPU_K6))) && (((((attr_type == TYPE_ISHIFT) || ((attr_type == TYPE_ISHIFT1) || ((attr_type == TYPE_ROTATE) || ((attr_type == TYPE_ROTATE1) || ((attr_type == TYPE_ALU1) || ((attr_type == TYPE_NEGNOT) || ((attr_type == TYPE_ALU) || ((attr_type == TYPE_ICMP) || ((attr_type == TYPE_TEST) || ((attr_type == TYPE_IMOVX) || ((attr_type == TYPE_INCDEC) || ((attr_type == TYPE_SETCC) || (attr_type == TYPE_LEA))))))))))))) || ((attr_type == TYPE_IMOV) && (attr_memory == MEMORY_NONE))) || (attr_type == TYPE_IMUL)) || (attr_type == TYPE_IDIV))) ? (64) : (0));
  accum |= (((((ix86_cpu) == (CPU_K6))) && ((attr_type == TYPE_CALL) || ((attr_type == TYPE_CALLV) || (attr_type == TYPE_IBR)))) ? (128) : (0));
  accum |= (((((ix86_cpu) == (CPU_K6))) && (((attr_type == TYPE_POP) || ((attr_memory == MEMORY_LOAD) || (attr_memory == MEMORY_BOTH))) || ((attr_type == TYPE_STR) && ((attr_memory == MEMORY_LOAD) || (attr_memory == MEMORY_BOTH))))) ? (256) : (0));
  accum |= (((((ix86_cpu) == (CPU_K6))) && (((attr_type == TYPE_LEA) || (attr_type == TYPE_STR)) || ((attr_type == TYPE_PUSH) || ((attr_memory == MEMORY_STORE) || (attr_memory == MEMORY_BOTH))))) ? (512) : (0));
  accum |= (((((ix86_cpu) == (CPU_K6))) && ((((attr_type == TYPE_FOP) || ((attr_type == TYPE_FMOV) || ((attr_type == TYPE_FCMP) || (attr_type == TYPE_FISTP)))) || (attr_type == TYPE_FMUL)) || ((attr_type == TYPE_FDIV) || (attr_type == TYPE_FPSPC)))) ? (1024) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && ((attr_athlon_decode == ATHLON_DECODE_VECTOR) || (attr_athlon_decode == ATHLON_DECODE_DIRECT))) ? (2048) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && (attr_athlon_decode == ATHLON_DECODE_DIRECT)) ? (4096) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && (((((attr_type == TYPE_ALU1) || ((attr_type == TYPE_NEGNOT) || ((attr_type == TYPE_ALU) || ((attr_type == TYPE_ICMP) || ((attr_type == TYPE_TEST) || ((attr_type == TYPE_IMOV) || ((attr_type == TYPE_IMOVX) || ((attr_type == TYPE_LEA) || ((attr_type == TYPE_INCDEC) || ((attr_type == TYPE_ISHIFT) || ((attr_type == TYPE_ISHIFT1) || ((attr_type == TYPE_ROTATE) || ((attr_type == TYPE_ROTATE1) || ((attr_type == TYPE_IBR) || ((attr_type == TYPE_CALL) || ((attr_type == TYPE_CALLV) || ((attr_type == TYPE_ICMOV) || ((attr_type == TYPE_CLD) || ((attr_type == TYPE_POP) || ((attr_type == TYPE_SETCC) || (attr_type == TYPE_PUSH))))))))))))))))))))) || (attr_type == TYPE_STR)) || (attr_type == TYPE_IMUL)) || (attr_type == TYPE_IDIV))) ? (8192) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && ((attr_type == TYPE_IMUL) || (attr_type == TYPE_IDIV))) ? (16384) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && ((((((((attr_type == TYPE_FPSPC) || (attr_type == TYPE_FDIV)) || ((attr_type == TYPE_FOP) || ((attr_type == TYPE_FMUL) || (attr_type == TYPE_FISTP)))) || ((attr_type == TYPE_FMOV) && ((attr_memory == MEMORY_LOAD) && (attr_mode == MODE_XF)))) || ((attr_type == TYPE_FMOV) || (attr_type == TYPE_FSGN))) || ((attr_type == TYPE_FCMP) && (attr_athlon_decode == ATHLON_DECODE_DIRECT))) || ((attr_type == TYPE_FCMP) && (attr_athlon_decode == ATHLON_DECODE_VECTOR))) || (attr_type == TYPE_FCMOV))) ? (32768) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && (attr_athlon_fpunits == ATHLON_FPUNITS_MUL)) ? (65536) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && (attr_athlon_fpunits == ATHLON_FPUNITS_ADD)) ? (131072) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && ((attr_athlon_fpunits == ATHLON_FPUNITS_MULADD) || ((attr_athlon_fpunits == ATHLON_FPUNITS_MUL) || (attr_athlon_fpunits == ATHLON_FPUNITS_ADD)))) ? (262144) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && ((attr_memory == MEMORY_LOAD) || (attr_memory == MEMORY_BOTH))) ? (1048576) : (0));
  accum |= (((((ix86_cpu) == (CPU_ATHLON))) && (attr_athlon_fpunits == ATHLON_FPUNITS_STORE)) ? (524288) : (0));

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

extern enum attr_athlon_fpunits get_attr_athlon_fpunits PARAMS ((rtx));
enum attr_athlon_fpunits
get_attr_athlon_fpunits (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 643:
    case 642:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else if ((mult_operator (operands[3], TFmode)) || (get_attr_type (insn) == TYPE_FDIV))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else if ((mult_operator (operands[3], XFmode)) || (get_attr_type (insn) == TYPE_FDIV))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else if (((which_alternative != 2) && (mult_operator (operands[3], DFmode))) || (get_attr_type (insn) == TYPE_FDIV))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else if ((mult_operator (operands[3], DFmode)) || (get_attr_type (insn) == TYPE_FDIV))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else if (((which_alternative != 2) && (mult_operator (operands[3], SFmode))) || (get_attr_type (insn) == TYPE_FDIV))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else if ((mult_operator (operands[3], SFmode)) || (get_attr_type (insn) == TYPE_FDIV))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 555:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], TFmode)))
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else
        {
	  return ATHLON_FPUNITS_MUL;
        }

    case 554:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], XFmode)))
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else
        {
	  return ATHLON_FPUNITS_MUL;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else if ((which_alternative == 0) && (mult_operator (operands[3], SFmode)))
        {
	  return ATHLON_FPUNITS_MUL;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], SFmode)))
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else
        {
	  return ATHLON_FPUNITS_MUL;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if ((which_alternative == 1) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return ATHLON_FPUNITS_ANY;
        }
      else if ((which_alternative == 1) && ((register_operand (operands[1], SImode)) || (immediate_operand (operands[1], VOIDmode))))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if (which_alternative != 0)
        {
	  return ATHLON_FPUNITS_MULADD;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if ((which_alternative == 0) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return ATHLON_FPUNITS_ANY;
        }
      else if ((which_alternative == 0) && ((register_operand (operands[1], SImode)) || (immediate_operand (operands[1], VOIDmode))))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if (which_alternative == 0)
        {
	  return ATHLON_FPUNITS_MULADD;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
      extract_insn_cached (insn);
      if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if (get_attr_memory (insn) == MEMORY_LOAD)
        {
	  return ATHLON_FPUNITS_ANY;
        }
      else if ((register_operand (operands[1], SImode)) || (immediate_operand (operands[1], VOIDmode)))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else
        {
	  return ATHLON_FPUNITS_MULADD;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return ATHLON_FPUNITS_ANY;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((register_operand (operands[1], SImode)) || (immediate_operand (operands[1], VOIDmode))))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ATHLON_FPUNITS_MULADD;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH)))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (get_attr_memory (insn) == MEMORY_LOAD))
        {
	  return ATHLON_FPUNITS_ANY;
        }
      else if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((register_operand (operands[1], SImode)) || (immediate_operand (operands[1], VOIDmode))))
        {
	  return ATHLON_FPUNITS_STORE;
        }
      else if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return ATHLON_FPUNITS_MULADD;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ATHLON_FPUNITS_ADD;
        }
      else
        {
	  return ATHLON_FPUNITS_NONE;
        }

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      return ATHLON_FPUNITS_ADD;

    case 645:
    case 644:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
      return ATHLON_FPUNITS_MUL;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return ATHLON_FPUNITS_NONE;

    }
}

extern enum attr_athlon_decode get_attr_athlon_decode PARAMS ((rtx));
enum attr_athlon_decode
get_attr_athlon_decode (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ATHLON_DECODE_DIRECT;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && (which_alternative == 1)))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || ((which_alternative == 4) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 1) || (memory_operand (operands[1], VOIDmode)))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 714:
    case 643:
    case 641:
    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (memory_operand (operands[1], VOIDmode)))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (memory_operand (operands[1], VOIDmode))
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case 926:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ATHLON_DECODE_VECTOR;
        }
      else
        {
	  return ATHLON_DECODE_DIRECT;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 854:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 645:
    case 644:
    case 642:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 543:
    case 542:
    case 541:
    case 540:
    case 539:
    case 538:
    case 537:
    case 536:
    case 533:
    case 532:
    case 531:
    case 530:
    case 528:
    case 527:
    case 526:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
    case 225:
    case 179:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 79:
    case 78:
    case 75:
    case 41:
    case 40:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
    case 30:
    case 31:
    case 32:
    case 33:
    case 34:
    case 35:
    case 36:
    case 47:
    case 86:
    case 160:
    case 412:
    case 534:
    case 535:
    case 787:
    case 789:
    case 791:
    case 924:
      return ATHLON_DECODE_VECTOR;

    default:
      return ATHLON_DECODE_DIRECT;

    }
}

extern enum attr_fp_int_src get_attr_fp_int_src PARAMS ((rtx));
enum attr_fp_int_src
get_attr_fp_int_src (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 161:
    case 162:
    case 163:
    case 164:
    case 165:
    case 166:
    case 167:
    case 168:
    case 169:
    case 170:
    case 171:
    case 172:
    case 173:
    case 174:
    case 175:
    case 176:
    case 177:
    case 178:
    case 559:
    case 560:
    case 564:
    case 565:
    case 570:
    case 571:
    case 572:
    case 573:
      return FP_INT_SRC_TRUE;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return FP_INT_SRC_FALSE;

    }
}

extern enum attr_imm_disp get_attr_imm_disp PARAMS ((rtx));
enum attr_imm_disp
get_attr_imm_disp (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 1) && (const0_operand (operands[2], DImode))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode)))) || ((which_alternative == 0) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode)))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 1) && (const0_operand (operands[2], SImode))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode)))) || ((which_alternative == 0) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode)))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if (((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 422:
    case 421:
    case 420:
    case 419:
    case 418:
    case 417:
    case 415:
    case 413:
    case 409:
    case 408:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_ISHIFT)) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 292:
    case 288:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 217:
      extract_insn_cached (insn);
      if ((! (incdec_operand (operands[2], QImode))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 223:
    case 222:
    case 221:
    case 219:
    case 218:
    case 216:
      extract_insn_cached (insn);
      if ((! (incdec_operand (operands[2], QImode))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 220:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
      extract_insn_cached (insn);
      if ((! (incdec_operand (operands[2], HImode))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
      extract_insn_cached (insn);
      if ((! (incdec_operand (operands[2], SImode))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 200:
    case 199:
    case 198:
    case 197:
      extract_insn_cached (insn);
      if ((! (incdec_operand (operands[2], DImode))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 202:
    case 201:
    case 196:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 497:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 484:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 471:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 447:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 348:
    case 347:
    case 346:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 318:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 296:
    case 294:
    case 293:
    case 291:
    case 290:
    case 289:
    case 287:
    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
    case 244:
    case 243:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
      extract_insn_cached (insn);
      if ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[2], VOIDmode)))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 115:
    case 112:
    case 109:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 1)
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  return IMM_DISP_UNKNOWN;
        }
      else if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode)))))))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 714:
    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return IMM_DISP_UNKNOWN;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if (((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 70:
    case 66:
    case 65:
    case 50:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMOV) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))) && ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case 498:
    case 496:
    case 485:
    case 483:
    case 472:
    case 470:
    case 448:
    case 446:
    case 407:
    case 405:
    case 403:
    case 402:
    case 399:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 242:
    case 111:
    case 108:
    case 106:
    case 86:
    case 85:
    case 84:
    case 81:
    case 80:
    case 74:
    case 73:
    case 72:
    case 68:
    case 67:
    case 62:
    case 61:
    case 60:
    case 56:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 47:
    case 46:
    case 45:
    case 43:
    case 42:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      extract_insn_cached (insn);
      if ((memory_displacement_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode)))
        {
	  return IMM_DISP_TRUE;
        }
      else
        {
	  return IMM_DISP_FALSE;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 854:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 75:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      return IMM_DISP_UNKNOWN;

    default:
      return IMM_DISP_FALSE;

    }
}

extern int get_attr_length_address PARAMS ((rtx));
int
get_attr_length_address (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_constrain_insn_cached (insn);
      if (constant_call_address_operand (operands[1], VOIDmode))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_constrain_insn_cached (insn);
      if (constant_call_address_operand (operands[0], VOIDmode))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 1)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 714:
    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_address_default (insn);
        }

    case 45:
    case 46:
    case 51:
    case 52:
    case 67:
    case 68:
    case 84:
    case 85:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 105:
    case 104:
    case 99:
    case 98:
    case 97:
    case 96:
    case 95:
    case 92:
    case 91:
    case 90:
    case 75:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
    case 854:
      return 0;

    default:
      extract_constrain_insn_cached (insn);
      return ix86_attr_length_address_default (insn);

    }
}

extern int get_attr_length_immediate PARAMS ((rtx));
int
get_attr_length_immediate (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[1], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], DImode))))
        {
	  return 0;
        }
      else if (which_alternative == 0)
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], SImode))))
        {
	  return 0;
        }
      else if (which_alternative == 0)
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }

    case 563:
    case 558:
      extract_constrain_insn_cached (insn);
      if (get_attr_unit (insn) == UNIT_SSE)
        {
	  return 0;
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 562:
    case 557:
      extract_constrain_insn_cached (insn);
      if ((get_attr_unit (insn) == UNIT_I387) || (get_attr_unit (insn) == UNIT_SSE))
        {
	  return 0;
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

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
    case 561:
    case 560:
    case 559:
    case 556:
      extract_constrain_insn_cached (insn);
      if (get_attr_unit (insn) == UNIT_I387)
        {
	  return 0;
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[0], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  return 0;
        }
      else if ((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_ISHIFT))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 0;
        }
      else if ((which_alternative == 0) && ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_ISHIFT))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 0;
        }
      else if ((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_ISHIFT))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 292:
    case 288:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return 0;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return 0;
        }

    case 223:
    case 222:
    case 221:
    case 219:
    case 218:
    case 217:
    case 216:
      extract_constrain_insn_cached (insn);
      if (incdec_operand (operands[2], QImode))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if ((incdec_operand (operands[2], QImode)) || (which_alternative == 3))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 220:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
      extract_constrain_insn_cached (insn);
      if (incdec_operand (operands[2], HImode))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((incdec_operand (operands[2], HImode)) || (which_alternative == 2))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
      extract_constrain_insn_cached (insn);
      if (incdec_operand (operands[2], SImode))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))))
        {
	  return 0;
        }
      else if (get_attr_type (insn) == TYPE_ALU)
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))))
        {
	  return 0;
        }
      else if (get_attr_type (insn) == TYPE_ALU)
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 200:
    case 199:
    case 198:
    case 197:
      extract_constrain_insn_cached (insn);
      if (incdec_operand (operands[2], DImode))
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))))
        {
	  return 0;
        }
      else if (get_attr_type (insn) == TYPE_ALU)
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 3) || (which_alternative == 4))) || ((which_alternative != 3) && (which_alternative != 4)))
        {
	  return 0;
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11))))))
        {
	  return 0;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 1)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  if (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))
	    {
	      return 0;
	    }
	  else
	    {
	      return ix86_attr_length_immediate_default(insn,0);
	    }
        }
      else if (which_alternative == 1)
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 8;
        }
      else
        {
	  if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))) || ((which_alternative == 4) || (((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))) || ((which_alternative == 5) || (which_alternative == 6)))))
	    {
	      return 0;
	    }
	  else
	    {
	      return ix86_attr_length_immediate_default(insn,0);
	    }
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }

    case 70:
    case 66:
    case 65:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_IMOVX)
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else if (get_attr_type (insn) == TYPE_IMOV)
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2)))))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else if (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2))))
        {
	  return ix86_attr_length_immediate_default(insn,1);
        }
      else if (get_attr_type (insn) == TYPE_IMOV)
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 545:
    case 544:
    case 283:
    case 282:
    case 281:
    case 279:
    case 278:
    case 277:
    case 276:
    case 86:
    case 74:
    case 73:
    case 72:
    case 61:
    case 60:
    case 55:
    case 54:
    case 53:
    case 47:
      extract_constrain_insn_cached (insn);
      return ix86_attr_length_immediate_default(insn,0);

    case 84:
    case 67:
    case 51:
    case 45:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 3) && (which_alternative != 4)) && (((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7))))) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 0;
        }
      else if ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))
        {
	  return ix86_attr_length_immediate_default(insn,0);
        }
      else
        {
	  return /* Update immediate_length and other attributes! */
		      abort(),1;
        }

    case 547:
    case 546:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 297:
    case 296:
    case 295:
    case 294:
    case 293:
    case 291:
    case 290:
    case 289:
    case 287:
    case 261:
    case 260:
    case 259:
    case 248:
    case 247:
    case 246:
    case 245:
    case 244:
    case 243:
    case 242:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 112:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 79:
    case 78:
    case 77:
    case 69:
    case 64:
    case 63:
    case 58:
    case 57:
    case 49:
    case 48:
    case 41:
    case 40:
    case 39:
    case 38:
    case 37:
    case 17:
    case 16:
    case 15:
    case 13:
    case 12:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      extract_constrain_insn_cached (insn);
      return ix86_attr_length_immediate_default(insn,1);

    case 0:
    case 3:
    case 6:
    case 9:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 520:
    case 515:
    case 504:
    case 503:
    case 43:
    case 81:
    case 280:
    case 298:
    case 299:
    case 321:
    case 340:
      return 1;

    case 528:
      return 2;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern enum attr_memory get_attr_memory PARAMS ((rtx));
enum attr_memory
get_attr_memory (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 714:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[1], VOIDmode))
        {
	  return MEMORY_NONE;
        }
      else
        {
	  return MEMORY_LOAD;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], DImode))))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((which_alternative != 1) || (! (const0_operand (operands[2], DImode)))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], SImode))))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((which_alternative != 1) || (! (const0_operand (operands[2], SImode)))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return MEMORY_UNKNOWN;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || ((memory_operand (operands[2], VOIDmode)) || (((which_alternative == 2) || (which_alternative == 3)) && (memory_operand (operands[3], VOIDmode)))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 640:
    case 639:
    case 637:
      extract_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || ((memory_operand (operands[2], VOIDmode)) || (memory_operand (operands[3], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[0], VOIDmode))
        {
	  return MEMORY_NONE;
        }
      else
        {
	  return MEMORY_LOAD;
        }

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || ((which_alternative == 1) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 417:
    case 414:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (memory_operand (operands[2], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 292:
    case 288:
      extract_constrain_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((which_alternative == 0) || (which_alternative == 1)) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 217:
      extract_insn_cached (insn);
      if (((! (incdec_operand (operands[2], QImode))) && (memory_operand (operands[1], VOIDmode))) || ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode))))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || ((incdec_operand (operands[2], QImode)) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 3)
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (memory_operand (operands[2], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 420:
    case 209:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (memory_operand (operands[2], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (memory_operand (operands[2], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (memory_operand (operands[2], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (memory_operand (operands[2], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 170:
    case 167:
    case 164:
    case 161:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 112:
    case 109:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (memory_operand (operands[1], VOIDmode))) || ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode))))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative == 3) || (which_alternative == 4)))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 1)
        {
	  return MEMORY_UNKNOWN;
        }
      else
        {
	  if (memory_operand (operands[1], VOIDmode))
	    {
	      return MEMORY_BOTH;
	    }
	  else
	    {
	      return MEMORY_STORE;
	    }
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  return MEMORY_UNKNOWN;
        }
      else if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))) && ((((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))) && ((which_alternative != 5) && (which_alternative != 6)))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return MEMORY_UNKNOWN;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || ((((which_alternative == 2) || (which_alternative == 3)) && ((which_alternative != 2) && (which_alternative != 3))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return MEMORY_UNKNOWN;
        }
      else
        {
	  if (memory_operand (operands[1], VOIDmode))
	    {
	      return MEMORY_BOTH;
	    }
	  else
	    {
	      return MEMORY_STORE;
	    }
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || ((((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))) && ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 70:
    case 66:
    case 65:
      extract_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((! (get_attr_type (insn) == TYPE_IMOV)) && (! (get_attr_type (insn) == TYPE_IMOVX))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2)))))) && (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((! (get_attr_type (insn) == TYPE_IMOV)) && ((((which_alternative == 0) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_HIMODE_MATH) == (0)))) || (((which_alternative == 1) || (which_alternative == 2)) && (aligned_operand (operands[1], HImode)))) || ((! ((TARGET_MOVX) != (0))) || ((which_alternative != 0) && (which_alternative != 2))))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 1035:
    case 1034:
    case 1033:
    case 1032:
    case 1031:
    case 1019:
    case 1018:
    case 1017:
    case 1016:
    case 1015:
    case 1014:
    case 1013:
    case 1012:
    case 1011:
    case 1010:
    case 1009:
    case 1008:
    case 1007:
    case 1006:
    case 1005:
    case 1004:
    case 1003:
    case 1002:
    case 1001:
    case 1000:
    case 999:
    case 998:
    case 997:
    case 996:
    case 995:
    case 994:
    case 993:
    case 992:
    case 991:
    case 990:
    case 961:
    case 960:
    case 959:
    case 958:
    case 957:
    case 930:
    case 929:
    case 928:
    case 927:
    case 926:
    case 925:
    case 924:
    case 923:
    case 922:
    case 921:
    case 920:
    case 919:
    case 918:
    case 917:
    case 916:
    case 915:
    case 914:
    case 913:
    case 912:
    case 911:
    case 910:
    case 909:
    case 908:
    case 907:
    case 906:
    case 905:
    case 898:
    case 897:
    case 880:
    case 879:
    case 877:
    case 876:
    case 875:
    case 874:
    case 873:
    case 871:
    case 870:
    case 866:
    case 865:
    case 849:
    case 848:
    case 847:
    case 846:
    case 845:
    case 844:
    case 822:
    case 821:
    case 820:
    case 791:
    case 790:
    case 789:
    case 788:
    case 787:
    case 786:
    case 785:
    case 784:
    case 783:
    case 782:
    case 781:
    case 780:
    case 779:
    case 778:
    case 777:
    case 744:
    case 743:
    case 742:
    case 741:
    case 740:
    case 739:
    case 730:
    case 729:
    case 728:
    case 727:
    case 726:
    case 725:
    case 724:
    case 723:
    case 722:
    case 721:
    case 720:
    case 719:
    case 718:
    case 717:
    case 716:
    case 715:
    case 713:
    case 712:
    case 711:
    case 710:
    case 709:
    case 691:
    case 690:
    case 689:
    case 688:
    case 687:
    case 686:
    case 685:
    case 684:
    case 683:
    case 682:
    case 657:
    case 654:
    case 651:
    case 648:
    case 586:
    case 583:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 172:
    case 169:
    case 166:
    case 163:
    case 155:
    case 154:
    case 150:
    case 149:
    case 145:
    case 143:
    case 141:
    case 139:
    case 137:
    case 136:
    case 135:
    case 132:
    case 131:
    case 130:
    case 129:
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
    case 117:
    case 116:
    case 115:
    case 113:
    case 110:
    case 107:
    case 86:
    case 74:
    case 73:
    case 72:
    case 69:
    case 64:
    case 63:
    case 61:
    case 60:
    case 55:
    case 54:
    case 53:
    case 47:
      extract_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))))
        {
	  return MEMORY_NONE;
        }
      else if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (((((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))) && ((((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) || ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 7)))) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && (memory_operand (operands[2], VOIDmode))))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      extract_insn_cached (insn);
      if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case 500:
    case 499:
    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_BOTH;
        }
      else
        {
	  return MEMORY_LOAD;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (memory_operand (operands[1], VOIDmode))
        {
	  return MEMORY_BOTH;
        }
      else
        {
	  return MEMORY_STORE;
        }

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      extract_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) || (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 635:
    case 634:
    case 633:
    case 632:
    case 631:
    case 630:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 75:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
    case 850:
    case 853:
    case 1020:
    case 1021:
    case 1022:
      return MEMORY_UNKNOWN;

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 606:
    case 607:
    case 608:
    case 609:
    case 610:
    case 611:
    case 612:
    case 613:
    case 614:
    case 615:
    case 616:
    case 617:
      return MEMORY_BOTH;

    case 45:
    case 51:
    case 67:
    case 84:
    case 618:
    case 619:
    case 620:
    case 621:
    case 622:
    case 623:
    case 624:
    case 625:
    case 626:
    case 627:
    case 628:
    case 629:
    case 852:
    case 854:
      return MEMORY_STORE;

    case 46:
    case 52:
    case 68:
    case 85:
    case 544:
    case 545:
    case 546:
    case 547:
    case 851:
      return MEMORY_LOAD;

    case 645:
    case 644:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 636:
    case 638:
    case 769:
    case 770:
    case 813:
    case 814:
    case 864:
    case 881:
    case 882:
    case 883:
    case 884:
    case 953:
      return MEMORY_NONE;

    default:
      extract_insn_cached (insn);
      if ((memory_operand (operands[0], VOIDmode)) && (memory_operand (operands[1], VOIDmode)))
        {
	  return MEMORY_BOTH;
        }
      else if (memory_operand (operands[0], VOIDmode))
        {
	  return MEMORY_STORE;
        }
      else if ((memory_operand (operands[1], VOIDmode)) || (memory_operand (operands[2], VOIDmode)))
        {
	  return MEMORY_LOAD;
        }
      else
        {
	  return MEMORY_NONE;
        }

    }
}

extern int get_attr_modrm PARAMS ((rtx));
int
get_attr_modrm (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[1], VOIDmode))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (const0_operand (operands[2], DImode))) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (const0_operand (operands[2], SImode))) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

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
    case 562:
    case 561:
    case 560:
    case 559:
    case 557:
    case 556:
      if (get_attr_unit (insn) == UNIT_I387)
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[0], VOIDmode))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 279:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 1;
        }
      else
        {
	  return 1;
        }

    case 223:
    case 222:
    case 221:
    case 219:
    case 218:
    case 217:
    case 216:
      extract_insn_cached (insn);
      if ((incdec_operand (operands[2], QImode)) && ((register_operand (operands[1], SImode)) || (register_operand (operands[1], HImode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && (incdec_operand (operands[2], QImode))) && ((register_operand (operands[1], SImode)) || (register_operand (operands[1], HImode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 220:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
      extract_insn_cached (insn);
      if ((incdec_operand (operands[2], HImode)) && ((register_operand (operands[1], SImode)) || (register_operand (operands[1], HImode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (incdec_operand (operands[2], HImode))) && ((register_operand (operands[1], SImode)) || (register_operand (operands[1], HImode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
      extract_insn_cached (insn);
      if ((incdec_operand (operands[2], SImode)) && ((register_operand (operands[1], SImode)) || (register_operand (operands[1], HImode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 200:
    case 199:
    case 198:
    case 197:
      extract_insn_cached (insn);
      if ((incdec_operand (operands[2], DImode)) && ((register_operand (operands[1], SImode)) || (register_operand (operands[1], HImode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 202:
    case 201:
    case 196:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_INCDEC) && ((register_operand (operands[1], SImode)) || (register_operand (operands[1], HImode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 643:
    case 642:
    case 641:
    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 124:
    case 123:
    case 122:
      extract_constrain_insn_cached (insn);
      if ((! (((ix86_cpu) == (CPU_K6)))) && (which_alternative == 0))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (((which_alternative == 3) || (which_alternative == 4)) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode)))))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  if (((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode)))) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 0;
        }
      else
        {
	  if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if (((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 74:
    case 73:
    case 72:
    case 61:
    case 55:
      extract_insn_cached (insn);
      if ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode)))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 70:
    case 66:
    case 65:
    case 50:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMOV) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 85:
    case 84:
    case 68:
    case 67:
    case 52:
    case 51:
    case 46:
    case 45:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else
        {
	  if ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode)))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))) && ((register_operand (operands[0], VOIDmode)) && (immediate_operand (operands[1], VOIDmode))))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if (! (memory_operand (operands[0], VOIDmode)))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (! (memory_operand (operands[1], VOIDmode)))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 276:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else if (which_alternative == 1)
        {
	  return 1;
        }
      else if (which_alternative == 2)
        {
	  return 0;
        }
      else if (which_alternative == 3)
        {
	  return 1;
        }
      else
        {
	  return 1;
        }

    case 277:
    case 278:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else if (which_alternative == 1)
        {
	  return 1;
        }
      else
        {
	  return 1;
        }

    case 585:
    case 582:
    case 552:
    case 549:
    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
    case 119:
    case 423:
    case 431:
    case 432:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 645:
    case 644:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
    case 555:
    case 554:
    case 551:
    case 548:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 160:
    case 159:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 105:
    case 104:
    case 95:
    case 90:
    case 34:
    case 31:
    case 29:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
    case 47:
    case 53:
    case 54:
    case 60:
    case 86:
    case 503:
    case 504:
    case 515:
    case 527:
    case 528:
    case 530:
    case 534:
    case 535:
    case 544:
    case 545:
    case 546:
    case 547:
    case 854:
      return 0;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

extern enum attr_mode get_attr_mode PARAMS ((rtx));
enum attr_mode
get_attr_mode (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return MODE_SF;
        }
      else
        {
	  return MODE_SI;
        }

    case 279:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return MODE_QI;
        }
      else
        {
	  return MODE_SI;
        }

    case 276:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_DI;
        }

    case 338:
    case 316:
    case 296:
    case 294:
    case 216:
    case 215:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return MODE_QI;
        }
      else
        {
	  return MODE_SI;
        }

    case 292:
    case 209:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return MODE_HI;
        }
      else
        {
	  return MODE_SI;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return MODE_SF;
        }
      else
        {
	  return MODE_DF;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return MODE_XF;
        }
      else
        {
	  return MODE_SI;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return MODE_DF;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return MODE_SI;
        }
      else if (which_alternative == 5)
        {
	  return MODE_TI;
        }
      else
        {
	  return MODE_DF;
        }

    case 91:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_DF;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_DF;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return MODE_SF;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return MODE_SI;
        }
      else if (which_alternative == 5)
        {
	  return MODE_TI;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))
        {
	  return MODE_SF;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_DI;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_SI;
        }
      else if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return MODE_DI;
        }
      else if (which_alternative == 4)
        {
	  return MODE_SI;
        }
      else if ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return MODE_DI;
        }
      else if (which_alternative == 8)
        {
	  return MODE_TI;
        }
      else
        {
	  return MODE_DI;
        }

    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return MODE_DI;
        }
      else if (which_alternative == 5)
        {
	  return MODE_TI;
        }
      else
        {
	  return MODE_DI;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_QI;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return MODE_SI;
        }
      else if (which_alternative == 6)
        {
	  return MODE_QI;
        }
      else if ((((TARGET_MOVX) != (0)) && (which_alternative == 2)) || (((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((TARGET_PARTIAL_REG_DEPENDENCY) != (0)) || (((TARGET_PARTIAL_REG_STALL) != (0)) && ((TARGET_QIMODE_MATH) == (0)))))))
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_QI;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) || ((((which_alternative == 1) || (which_alternative == 2)) && (aligned_operand (operands[1], HImode))) || ((which_alternative == 0) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_HIMODE_MATH) == (0))))))
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_HI;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return MODE_SI;
        }
      else if (which_alternative == 4)
        {
	  return MODE_DI;
        }
      else if (which_alternative == 5)
        {
	  return MODE_TI;
        }
      else
        {
	  return MODE_SI;
        }

    case 65:
    case 66:
    case 70:
      if (get_attr_type (insn) == TYPE_IMOVX)
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_QI;
        }

    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_SF;
        }
      else if (which_alternative == 1)
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_SF;
        }

    case 88:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_SF;
        }
      else if (which_alternative == 1)
        {
	  return MODE_DI;
        }
      else
        {
	  return MODE_SF;
        }

    case 92:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_DF;
        }
      else if (which_alternative == 1)
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_DF;
        }

    case 97:
    case 96:
    case 98:
    case 99:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_XF;
        }
      else
        {
	  return MODE_SI;
        }

    case 287:
    case 286:
    case 115:
    case 116:
    case 117:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_SI;
        }
      else
        {
	  return MODE_DI;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_SF;
        }
      else if (which_alternative == 1)
        {
	  return MODE_XF;
        }
      else
        {
	  return MODE_DF;
        }

    case 129:
    case 130:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_SF;
        }
      else
        {
	  return MODE_XF;
        }

    case 131:
    case 132:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_DF;
        }
      else
        {
	  return MODE_XF;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_DF;
        }
      else
        {
	  return MODE_SF;
        }

    case 417:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_HI;
        }
      else
        {
	  return MODE_SI;
        }

    case 420:
    case 406:
    case 421:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return MODE_QI;
        }
      else
        {
	  return MODE_SI;
        }

    case 855:
    case 856:
    case 857:
    case 858:
    case 859:
    case 860:
    case 861:
    case 862:
    case 863:
    case 865:
    case 866:
    case 867:
    case 868:
    case 869:
    case 870:
    case 871:
      return MODE_V2SF;

    case 689:
    case 753:
    case 754:
    case 755:
    case 756:
    case 757:
    case 758:
    case 759:
    case 760:
    case 885:
    case 887:
    case 889:
    case 891:
    case 893:
    case 895:
    case 897:
    case 899:
    case 900:
    case 905:
    case 906:
    case 909:
    case 911:
    case 912:
    case 915:
    case 930:
    case 1003:
    case 1004:
    case 1014:
    case 1015:
    case 1019:
    case 1026:
    case 1028:
    case 1030:
      return MODE_V2DF;

    case 682:
    case 683:
    case 684:
    case 690:
    case 691:
    case 713:
    case 714:
    case 715:
    case 716:
    case 717:
    case 718:
    case 721:
    case 723:
    case 724:
    case 725:
    case 726:
    case 730:
    case 731:
    case 733:
    case 735:
    case 737:
    case 739:
    case 741:
    case 743:
    case 745:
    case 746:
    case 747:
    case 748:
    case 749:
    case 750:
    case 751:
    case 752:
    case 769:
    case 770:
    case 771:
    case 772:
    case 777:
    case 778:
    case 779:
    case 781:
    case 783:
    case 784:
    case 929:
    case 1025:
    case 1027:
    case 1029:
    case 1031:
    case 1032:
      return MODE_V4SF;

    case 761:
    case 762:
    case 763:
    case 764:
    case 765:
    case 766:
    case 767:
    case 768:
    case 872:
    case 873:
    case 874:
    case 875:
    case 876:
    case 877:
    case 878:
    case 879:
    case 880:
    case 907:
    case 908:
    case 910:
    case 913:
    case 914:
    case 916:
    case 917:
    case 918:
    case 919:
    case 920:
    case 931:
    case 932:
    case 933:
    case 934:
    case 935:
    case 936:
    case 937:
    case 938:
    case 939:
    case 940:
    case 941:
    case 942:
    case 943:
    case 944:
    case 945:
    case 946:
    case 947:
    case 948:
    case 949:
    case 950:
    case 951:
    case 952:
    case 953:
    case 954:
    case 955:
    case 956:
    case 957:
    case 958:
    case 959:
    case 960:
    case 961:
    case 962:
    case 963:
    case 964:
    case 965:
    case 966:
    case 967:
    case 968:
    case 969:
    case 970:
    case 971:
    case 972:
    case 973:
    case 974:
    case 975:
    case 976:
    case 977:
    case 978:
    case 979:
    case 980:
    case 981:
    case 982:
    case 983:
    case 984:
    case 985:
    case 986:
    case 987:
    case 988:
    case 989:
    case 990:
    case 991:
    case 992:
    case 993:
    case 994:
    case 995:
    case 996:
    case 997:
    case 998:
    case 999:
    case 1000:
    case 1001:
    case 1002:
    case 1005:
    case 1006:
    case 1007:
    case 1008:
    case 1009:
    case 1010:
    case 1011:
    case 1012:
    case 1013:
    case 1033:
      return MODE_TI;

    case 23:
    case 24:
    case 25:
    case 26:
    case 104:
    case 105:
    case 173:
    case 174:
    case 175:
    case 176:
    case 177:
    case 178:
    case 373:
    case 374:
    case 375:
    case 376:
    case 377:
    case 378:
    case 393:
    case 394:
    case 396:
    case 397:
    case 554:
    case 555:
    case 568:
    case 569:
    case 589:
    case 590:
    case 591:
    case 592:
    case 593:
    case 594:
    case 598:
    case 599:
    case 603:
    case 604:
    case 644:
    case 645:
      return MODE_XF;

    case 21:
    case 22:
    case 95:
    case 128:
    case 137:
    case 142:
    case 143:
    case 144:
    case 145:
    case 167:
    case 168:
    case 169:
    case 170:
    case 171:
    case 172:
    case 371:
    case 372:
    case 390:
    case 391:
    case 392:
    case 395:
    case 502:
    case 551:
    case 552:
    case 553:
    case 561:
    case 562:
    case 563:
    case 578:
    case 579:
    case 580:
    case 581:
    case 585:
    case 586:
    case 587:
    case 588:
    case 595:
    case 597:
    case 600:
    case 602:
    case 642:
    case 643:
    case 651:
    case 657:
    case 886:
    case 888:
    case 890:
    case 892:
    case 894:
    case 896:
    case 901:
    case 902:
    case 903:
    case 904:
    case 925:
    case 926:
    case 928:
    case 1016:
    case 1017:
    case 1018:
    case 1034:
    case 1035:
      return MODE_DF;

    case 19:
    case 20:
    case 90:
    case 133:
    case 136:
    case 138:
    case 139:
    case 140:
    case 141:
    case 161:
    case 162:
    case 163:
    case 164:
    case 165:
    case 166:
    case 370:
    case 389:
    case 501:
    case 548:
    case 549:
    case 550:
    case 556:
    case 557:
    case 558:
    case 566:
    case 567:
    case 574:
    case 575:
    case 576:
    case 577:
    case 582:
    case 583:
    case 584:
    case 596:
    case 601:
    case 648:
    case 654:
    case 727:
    case 728:
    case 729:
    case 732:
    case 734:
    case 736:
    case 738:
    case 740:
    case 742:
    case 744:
    case 773:
    case 774:
    case 775:
    case 776:
    case 780:
    case 782:
    case 785:
    case 786:
    case 787:
    case 788:
    case 789:
    case 790:
    case 791:
    case 898:
    case 927:
      return MODE_SF;

    case 18:
    case 27:
    case 28:
    case 31:
    case 32:
    case 33:
    case 34:
    case 35:
    case 36:
      return MODE_UNKNOWNFP;

    case 0:
    case 1:
    case 2:
    case 76:
    case 77:
    case 78:
    case 79:
    case 81:
    case 84:
    case 85:
    case 86:
    case 119:
    case 120:
    case 121:
    case 180:
    case 181:
    case 189:
    case 196:
    case 197:
    case 198:
    case 199:
    case 200:
    case 226:
    case 227:
    case 228:
    case 229:
    case 237:
    case 245:
    case 252:
    case 254:
    case 256:
    case 259:
    case 266:
    case 271:
    case 272:
    case 303:
    case 304:
    case 305:
    case 325:
    case 326:
    case 327:
    case 350:
    case 351:
    case 398:
    case 399:
    case 408:
    case 409:
    case 423:
    case 425:
    case 427:
    case 452:
    case 454:
    case 476:
    case 488:
    case 606:
    case 613:
    case 618:
    case 625:
    case 636:
    case 637:
    case 659:
    case 685:
    case 686:
    case 687:
    case 688:
    case 719:
    case 720:
    case 722:
    case 792:
    case 793:
    case 794:
    case 795:
    case 796:
    case 797:
    case 798:
    case 799:
    case 800:
    case 801:
    case 802:
    case 803:
    case 804:
    case 805:
    case 806:
    case 807:
    case 808:
    case 809:
    case 810:
    case 811:
    case 812:
    case 813:
    case 814:
    case 815:
    case 816:
    case 817:
    case 818:
    case 819:
    case 820:
    case 821:
    case 822:
    case 823:
    case 824:
    case 825:
    case 826:
    case 827:
    case 828:
    case 829:
    case 830:
    case 831:
    case 832:
    case 833:
    case 834:
    case 835:
    case 836:
    case 837:
    case 838:
    case 839:
    case 840:
    case 841:
    case 842:
    case 843:
    case 844:
    case 845:
    case 846:
    case 847:
    case 848:
    case 849:
    case 854:
    case 924:
      return MODE_DI;

    case 3:
    case 4:
    case 5:
    case 29:
    case 30:
    case 37:
    case 38:
    case 39:
    case 40:
    case 41:
    case 42:
    case 43:
    case 45:
    case 46:
    case 47:
    case 54:
    case 63:
    case 64:
    case 69:
    case 80:
    case 106:
    case 107:
    case 111:
    case 112:
    case 113:
    case 114:
    case 122:
    case 123:
    case 125:
    case 126:
    case 182:
    case 183:
    case 184:
    case 186:
    case 187:
    case 188:
    case 190:
    case 191:
    case 192:
    case 193:
    case 194:
    case 195:
    case 201:
    case 202:
    case 203:
    case 204:
    case 205:
    case 206:
    case 207:
    case 208:
    case 213:
    case 230:
    case 231:
    case 232:
    case 233:
    case 234:
    case 235:
    case 236:
    case 246:
    case 247:
    case 253:
    case 255:
    case 257:
    case 258:
    case 260:
    case 261:
    case 269:
    case 270:
    case 273:
    case 274:
    case 277:
    case 288:
    case 289:
    case 290:
    case 291:
    case 306:
    case 307:
    case 308:
    case 309:
    case 310:
    case 311:
    case 312:
    case 328:
    case 329:
    case 330:
    case 331:
    case 332:
    case 333:
    case 334:
    case 352:
    case 353:
    case 354:
    case 355:
    case 400:
    case 401:
    case 402:
    case 403:
    case 412:
    case 413:
    case 414:
    case 415:
    case 416:
    case 430:
    case 431:
    case 432:
    case 435:
    case 436:
    case 439:
    case 440:
    case 459:
    case 460:
    case 463:
    case 464:
    case 479:
    case 480:
    case 491:
    case 492:
    case 559:
    case 560:
    case 564:
    case 565:
    case 570:
    case 571:
    case 572:
    case 573:
    case 607:
    case 608:
    case 614:
    case 615:
    case 616:
    case 617:
    case 619:
    case 620:
    case 626:
    case 627:
    case 638:
    case 639:
    case 658:
    case 921:
    case 922:
    case 923:
      return MODE_SI;

    case 6:
    case 7:
    case 8:
    case 48:
    case 51:
    case 52:
    case 53:
    case 55:
    case 56:
    case 57:
    case 108:
    case 109:
    case 110:
    case 124:
    case 159:
    case 160:
    case 210:
    case 211:
    case 212:
    case 214:
    case 238:
    case 239:
    case 240:
    case 248:
    case 275:
    case 278:
    case 293:
    case 313:
    case 314:
    case 315:
    case 335:
    case 336:
    case 337:
    case 356:
    case 357:
    case 404:
    case 405:
    case 418:
    case 419:
    case 442:
    case 444:
    case 466:
    case 468:
    case 482:
    case 494:
    case 609:
    case 610:
    case 621:
    case 622:
    case 640:
      return MODE_HI;

    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 49:
    case 58:
    case 60:
    case 61:
    case 62:
    case 67:
    case 68:
    case 72:
    case 73:
    case 74:
    case 185:
    case 217:
    case 218:
    case 219:
    case 220:
    case 221:
    case 222:
    case 223:
    case 224:
    case 241:
    case 242:
    case 243:
    case 244:
    case 249:
    case 250:
    case 251:
    case 262:
    case 263:
    case 280:
    case 281:
    case 282:
    case 283:
    case 295:
    case 297:
    case 298:
    case 299:
    case 300:
    case 301:
    case 302:
    case 317:
    case 318:
    case 319:
    case 320:
    case 321:
    case 322:
    case 323:
    case 324:
    case 339:
    case 340:
    case 341:
    case 342:
    case 343:
    case 344:
    case 345:
    case 346:
    case 347:
    case 348:
    case 358:
    case 359:
    case 407:
    case 422:
    case 447:
    case 448:
    case 450:
    case 471:
    case 472:
    case 474:
    case 485:
    case 486:
    case 497:
    case 498:
    case 499:
    case 500:
    case 611:
    case 612:
    case 623:
    case 624:
    case 628:
    case 629:
    case 630:
    case 631:
    case 632:
    case 633:
    case 634:
    case 635:
      return MODE_QI;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return MODE_UNKNOWN;

    }
}

extern enum attr_ppro_uops get_attr_ppro_uops PARAMS ((rtx));
enum attr_ppro_uops
get_attr_ppro_uops (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (const0_operand (operands[2], DImode)))
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (const0_operand (operands[2], SImode)))
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return PPRO_UOPS_MANY;
        }
      else
        {
	  return PPRO_UOPS_FEW;
        }

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return PPRO_UOPS_MANY;
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 529:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if (memory_operand (operands[0], VOIDmode))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return PPRO_UOPS_MANY;
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return PPRO_UOPS_MANY;
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return PPRO_UOPS_MANY;
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return PPRO_UOPS_MANY;
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return PPRO_UOPS_MANY;
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 1)
        {
	  return PPRO_UOPS_MANY;
        }
      else
        {
	  return PPRO_UOPS_FEW;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  return PPRO_UOPS_MANY;
        }
      else if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 714:
    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return PPRO_UOPS_MANY;
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return PPRO_UOPS_MANY;
        }
      else
        {
	  return PPRO_UOPS_FEW;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 74:
    case 73:
    case 72:
    case 61:
    case 55:
      if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 70:
    case 66:
    case 65:
    case 50:
      if (get_attr_type (insn) == TYPE_IMOV)
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode))))))))
        {
	  if ((get_attr_memory (insn) == MEMORY_STORE) || (get_attr_memory (insn) == MEMORY_BOTH))
	    {
	      return PPRO_UOPS_FEW;
	    }
	  else
	    {
	      return PPRO_UOPS_ONE;
	    }
        }
      else if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 854:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
    case 543:
    case 542:
    case 541:
    case 540:
    case 539:
    case 538:
    case 537:
    case 533:
    case 532:
    case 531:
    case 528:
    case 527:
    case 526:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 179:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 75:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
    case 520:
    case 559:
    case 560:
    case 564:
    case 565:
    case 570:
    case 571:
    case 572:
    case 573:
      return PPRO_UOPS_MANY;

    case 1022:
    case 1021:
    case 1020:
    case 853:
    case 852:
    case 851:
    case 850:
    case 645:
    case 644:
    case 643:
    case 641:
    case 640:
    case 639:
    case 637:
    case 605:
    case 547:
    case 546:
    case 500:
    case 499:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 84:
    case 79:
    case 78:
    case 77:
    case 67:
    case 58:
    case 57:
    case 51:
    case 49:
    case 48:
    case 45:
    case 41:
    case 40:
    case 39:
    case 38:
    case 37:
    case 29:
    case 47:
    case 53:
    case 54:
    case 60:
    case 86:
    case 159:
    case 160:
    case 180:
    case 182:
    case 183:
    case 226:
    case 230:
    case 231:
    case 252:
    case 253:
    case 256:
    case 257:
    case 258:
    case 259:
    case 260:
    case 261:
    case 262:
    case 263:
    case 266:
    case 269:
    case 272:
    case 274:
    case 275:
    case 370:
    case 371:
    case 372:
    case 373:
    case 374:
    case 375:
    case 376:
    case 377:
    case 378:
    case 412:
    case 430:
    case 534:
    case 535:
    case 536:
      return PPRO_UOPS_FEW;

    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 814:
    case 813:
    case 770:
    case 769:
    case 638:
    case 636:
    case 545:
    case 544:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 85:
    case 68:
    case 52:
    case 46:
    case 30:
    case 530:
      return PPRO_UOPS_ONE;

    default:
      if (! (get_attr_memory (insn) == MEMORY_NONE))
        {
	  return PPRO_UOPS_FEW;
        }
      else
        {
	  return PPRO_UOPS_ONE;
        }

    }
}

extern enum attr_pent_pair get_attr_pent_pair PARAMS ((rtx));
enum attr_pent_pair
get_attr_pent_pair (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[1], VOIDmode))
        {
	  return PENT_PAIR_PV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (constant_call_address_operand (operands[0], VOIDmode))
        {
	  return PENT_PAIR_PV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 498:
    case 496:
    case 485:
    case 483:
      extract_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (const_int_1_operand (operands[1], VOIDmode))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 497:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 484:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
      extract_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (const_int_1_operand (operands[2], VOIDmode))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 472:
    case 470:
    case 448:
    case 446:
      extract_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (const_int_operand (operands[1], VOIDmode))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 474:
    case 473:
    case 471:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 447:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 427:
    case 426:
    case 425:
    case 424:
      extract_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (const_int_operand (operands[2], VOIDmode))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((which_alternative == 1) && (const_int_operand (operands[2], VOIDmode)))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2))
        {
	  return PENT_PAIR_UV;
        }
      else if ((get_attr_type (insn) == TYPE_ISHIFT) && (const_int_operand (operands[2], VOIDmode)))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 419:
    case 418:
      extract_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((get_attr_type (insn) == TYPE_ALU) || ((get_attr_type (insn) == TYPE_ISHIFT) && (const_int_operand (operands[2], VOIDmode))))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 417:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))
        {
	  if (which_alternative == 0)
	    {
	      return PENT_PAIR_PU;
	    }
	  else
	    {
	      return PENT_PAIR_UV;
	    }
        }
      else if ((get_attr_type (insn) == TYPE_ISHIFT) && (const_int_operand (operands[2], VOIDmode)))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))
        {
	  return PENT_PAIR_UV;
        }
      else if (const_int_operand (operands[2], VOIDmode))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))
        {
	  return PENT_PAIR_UV;
        }
      else if (const_int_operand (operands[2], VOIDmode))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 422:
    case 421:
    case 415:
    case 409:
      extract_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (get_attr_type (insn) == TYPE_ALU)
        {
	  return PENT_PAIR_UV;
        }
      else if ((get_attr_type (insn) == TYPE_ISHIFT) && (const_int_operand (operands[2], VOIDmode)))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU))
        {
	  return PENT_PAIR_UV;
        }
      else if ((get_attr_type (insn) == TYPE_ISHIFT) && (const_int_operand (operands[2], VOIDmode)))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 292:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 288:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) || ((which_alternative == 3) || (incdec_operand (operands[2], QImode))))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) || ((which_alternative == 2) || (incdec_operand (operands[2], HImode))))
        {
	  if ((which_alternative == 0) || (which_alternative == 1))
	    {
	      return PENT_PAIR_PU;
	    }
	  else
	    {
	      return PENT_PAIR_UV;
	    }
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 115:
    case 112:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (which_alternative != 0)
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 109:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (which_alternative != 0)
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))))))
	    {
	      return PENT_PAIR_PU;
	    }
	  else
	    {
	      return PENT_PAIR_UV;
	    }
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode)))) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode))))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if ((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0))))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 70:
    case 66:
    case 65:
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (get_attr_type (insn) == TYPE_IMOV)
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2))))))
        {
	  if (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2)))))
	    {
	      return PENT_PAIR_PU;
	    }
	  else
	    {
	      return PENT_PAIR_UV;
	    }
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (get_attr_type (insn) == TYPE_IMOV)
        {
	  if (((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) || (get_attr_mode (insn) == MODE_HI))
	    {
	      return PENT_PAIR_PU;
	    }
	  else
	    {
	      return PENT_PAIR_UV;
	    }
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 57:
    case 48:
      extract_insn_cached (insn);
      if (! (memory_operand (operands[1], VOIDmode)))
        {
	  return PENT_PAIR_PU;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 79:
    case 78:
    case 41:
    case 40:
      extract_insn_cached (insn);
      if (! (memory_operand (operands[0], VOIDmode)))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 77:
    case 58:
    case 49:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (! (memory_operand (operands[1], VOIDmode)))
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 405:
    case 337:
    case 336:
    case 335:
    case 315:
    case 314:
    case 313:
    case 293:
    case 240:
    case 239:
    case 238:
    case 214:
    case 212:
    case 211:
    case 210:
    case 108:
    case 56:
    case 55:
    case 52:
    case 51:
    case 8:
    case 7:
    case 6:
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else
        {
	  return PENT_PAIR_PU;
        }

    case 659:
    case 658:
    case 407:
    case 403:
    case 402:
    case 399:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 294:
    case 291:
    case 290:
    case 289:
    case 287:
    case 283:
    case 282:
    case 281:
    case 244:
    case 243:
    case 242:
    case 241:
    case 237:
    case 236:
    case 235:
    case 234:
    case 233:
    case 232:
    case 229:
    case 228:
    case 227:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 213:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 185:
    case 184:
    case 181:
    case 111:
    case 106:
    case 85:
    case 84:
    case 81:
    case 80:
    case 74:
    case 73:
    case 72:
    case 68:
    case 67:
    case 62:
    case 61:
    case 46:
    case 45:
    case 43:
    case 42:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if (get_attr_imm_disp (insn) == IMM_DISP_TRUE)
        {
	  return PENT_PAIR_NP;
        }
      else
        {
	  return PENT_PAIR_UV;
        }

    case 276:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return PENT_PAIR_UV;
        }
      else if (which_alternative == 1)
        {
	  return PENT_PAIR_NP;
        }
      else if (which_alternative == 2)
        {
	  return PENT_PAIR_UV;
        }
      else if (which_alternative == 3)
        {
	  return PENT_PAIR_NP;
        }
      else
        {
	  return PENT_PAIR_UV;
        }

    case 277:
    case 278:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return PENT_PAIR_UV;
        }
      else if (which_alternative == 1)
        {
	  return PENT_PAIR_NP;
        }
      else
        {
	  return PENT_PAIR_UV;
        }

    case 279:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return PENT_PAIR_UV;
        }
      else if (which_alternative == 1)
        {
	  return PENT_PAIR_NP;
        }
      else if (which_alternative == 2)
        {
	  return PENT_PAIR_UV;
        }
      else
        {
	  return PENT_PAIR_NP;
        }

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      return PENT_PAIR_PV;

    case 180:
    case 182:
    case 183:
    case 226:
    case 230:
    case 231:
    case 636:
    case 638:
      return PENT_PAIR_PU;

    case 547:
    case 546:
    case 545:
    case 544:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      return PENT_PAIR_UV;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return PENT_PAIR_NP;

    }
}

extern enum attr_pent_prefix get_attr_pent_prefix PARAMS ((rtx));
enum attr_pent_prefix
get_attr_pent_prefix (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 563:
    case 562:
    case 558:
    case 557:
      if ((get_attr_prefix_0f (insn) == 1) || (get_attr_unit (insn) == UNIT_SSE))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 3))))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 714:
    case 288:
    case 247:
    case 246:
    case 245:
    case 171:
    case 168:
    case 165:
    case 162:
    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 123:
    case 122:
      extract_constrain_insn_cached (insn);
      if ((((ix86_cpu) == (CPU_K6))) || (which_alternative != 0))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 417:
    case 135:
    case 115:
    case 112:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))) || ((((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative != 0) && ((which_alternative != 3) && (which_alternative != 4)))) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))))))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))) || ((which_alternative == 5) || (which_alternative == 6)))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 82:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative != 2) && (which_alternative != 3)) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 70:
    case 66:
    case 65:
      if (get_attr_type (insn) == TYPE_IMOVX)
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2)))))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) || (get_attr_mode (insn) == MODE_HI))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 585:
    case 582:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return PENT_PREFIX_TRUE;
        }
      else
        {
	  return PENT_PREFIX_FALSE;
        }

    case 1035:
    case 1034:
    case 1033:
    case 1032:
    case 1031:
    case 1030:
    case 1029:
    case 1028:
    case 1027:
    case 1026:
    case 1025:
    case 1022:
    case 1021:
    case 1020:
    case 1019:
    case 1018:
    case 1017:
    case 1016:
    case 1015:
    case 1014:
    case 1013:
    case 1012:
    case 1011:
    case 1010:
    case 1009:
    case 1008:
    case 1007:
    case 1006:
    case 1005:
    case 1004:
    case 1003:
    case 1002:
    case 1001:
    case 1000:
    case 999:
    case 998:
    case 997:
    case 996:
    case 995:
    case 994:
    case 993:
    case 992:
    case 991:
    case 990:
    case 989:
    case 988:
    case 987:
    case 986:
    case 985:
    case 984:
    case 983:
    case 982:
    case 981:
    case 980:
    case 979:
    case 978:
    case 977:
    case 976:
    case 975:
    case 974:
    case 973:
    case 972:
    case 971:
    case 970:
    case 969:
    case 968:
    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 961:
    case 960:
    case 959:
    case 958:
    case 957:
    case 956:
    case 955:
    case 954:
    case 953:
    case 952:
    case 951:
    case 950:
    case 949:
    case 948:
    case 947:
    case 946:
    case 945:
    case 944:
    case 943:
    case 942:
    case 941:
    case 940:
    case 939:
    case 938:
    case 937:
    case 936:
    case 935:
    case 934:
    case 933:
    case 932:
    case 931:
    case 930:
    case 929:
    case 928:
    case 927:
    case 926:
    case 925:
    case 924:
    case 923:
    case 922:
    case 921:
    case 920:
    case 919:
    case 918:
    case 917:
    case 916:
    case 915:
    case 914:
    case 913:
    case 912:
    case 911:
    case 910:
    case 909:
    case 908:
    case 907:
    case 906:
    case 905:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 898:
    case 897:
    case 896:
    case 895:
    case 894:
    case 893:
    case 892:
    case 891:
    case 890:
    case 889:
    case 888:
    case 887:
    case 886:
    case 885:
    case 884:
    case 883:
    case 882:
    case 881:
    case 880:
    case 879:
    case 878:
    case 877:
    case 876:
    case 875:
    case 874:
    case 873:
    case 872:
    case 871:
    case 870:
    case 869:
    case 868:
    case 867:
    case 866:
    case 865:
    case 864:
    case 863:
    case 862:
    case 861:
    case 860:
    case 859:
    case 858:
    case 857:
    case 856:
    case 855:
    case 853:
    case 852:
    case 851:
    case 850:
    case 849:
    case 848:
    case 847:
    case 846:
    case 845:
    case 844:
    case 843:
    case 842:
    case 841:
    case 840:
    case 839:
    case 838:
    case 837:
    case 836:
    case 835:
    case 834:
    case 833:
    case 832:
    case 831:
    case 830:
    case 829:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 822:
    case 821:
    case 820:
    case 819:
    case 818:
    case 817:
    case 816:
    case 815:
    case 814:
    case 813:
    case 812:
    case 811:
    case 810:
    case 809:
    case 808:
    case 807:
    case 806:
    case 805:
    case 804:
    case 803:
    case 802:
    case 801:
    case 800:
    case 799:
    case 798:
    case 797:
    case 796:
    case 795:
    case 794:
    case 793:
    case 792:
    case 791:
    case 790:
    case 789:
    case 788:
    case 787:
    case 786:
    case 785:
    case 784:
    case 783:
    case 782:
    case 781:
    case 780:
    case 779:
    case 778:
    case 777:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 770:
    case 769:
    case 768:
    case 767:
    case 766:
    case 765:
    case 764:
    case 763:
    case 762:
    case 761:
    case 760:
    case 759:
    case 758:
    case 757:
    case 756:
    case 755:
    case 754:
    case 753:
    case 752:
    case 751:
    case 750:
    case 749:
    case 748:
    case 747:
    case 746:
    case 745:
    case 744:
    case 743:
    case 742:
    case 741:
    case 740:
    case 739:
    case 738:
    case 737:
    case 736:
    case 735:
    case 734:
    case 733:
    case 732:
    case 731:
    case 730:
    case 729:
    case 728:
    case 727:
    case 726:
    case 725:
    case 724:
    case 723:
    case 722:
    case 721:
    case 720:
    case 719:
    case 718:
    case 717:
    case 716:
    case 715:
    case 713:
    case 712:
    case 711:
    case 710:
    case 709:
    case 691:
    case 690:
    case 689:
    case 688:
    case 687:
    case 686:
    case 685:
    case 684:
    case 683:
    case 682:
    case 657:
    case 654:
    case 651:
    case 648:
    case 640:
    case 639:
    case 637:
    case 635:
    case 634:
    case 633:
    case 632:
    case 631:
    case 630:
    case 629:
    case 628:
    case 627:
    case 626:
    case 625:
    case 622:
    case 621:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 610:
    case 609:
    case 586:
    case 583:
    case 553:
    case 550:
    case 536:
    case 502:
    case 501:
    case 500:
    case 499:
    case 494:
    case 482:
    case 468:
    case 466:
    case 444:
    case 442:
    case 430:
    case 419:
    case 418:
    case 412:
    case 405:
    case 404:
    case 357:
    case 356:
    case 337:
    case 336:
    case 335:
    case 315:
    case 314:
    case 313:
    case 293:
    case 292:
    case 278:
    case 275:
    case 248:
    case 240:
    case 239:
    case 238:
    case 214:
    case 212:
    case 211:
    case 210:
    case 172:
    case 169:
    case 166:
    case 163:
    case 160:
    case 159:
    case 155:
    case 154:
    case 150:
    case 149:
    case 137:
    case 128:
    case 126:
    case 125:
    case 124:
    case 121:
    case 120:
    case 117:
    case 116:
    case 113:
    case 110:
    case 109:
    case 108:
    case 107:
    case 69:
    case 64:
    case 63:
    case 57:
    case 56:
    case 55:
    case 53:
    case 52:
    case 51:
    case 48:
    case 36:
    case 33:
    case 8:
    case 7:
    case 6:
      return PENT_PREFIX_TRUE;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return PENT_PREFIX_FALSE;

    }
}

extern int get_attr_prefix_0f PARAMS ((rtx));
int
get_attr_prefix_0f (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 563:
    case 558:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_SSEADD) || ((mult_operator (operands[3], SFmode)) || (get_attr_type (insn) == TYPE_SSEDIV)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 562:
    case 557:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_SSEADD) || (((which_alternative == 2) && (mult_operator (operands[3], SFmode))) || (get_attr_type (insn) == TYPE_SSEDIV)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 0;
        }
      else
        {
	  return 0;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 248:
    case 247:
    case 246:
    case 245:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 3))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 714:
    case 292:
    case 288:
    case 171:
    case 168:
    case 165:
    case 162:
    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 135:
    case 115:
    case 112:
    case 109:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))) || ((which_alternative == 5) || (which_alternative == 6)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 82:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative != 2) && (which_alternative != 3)) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 70:
    case 66:
    case 65:
      if (get_attr_type (insn) == TYPE_IMOVX)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 585:
    case 582:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 122:
    case 123:
    case 124:
      extract_constrain_insn_cached (insn);
      if ((! (((ix86_cpu) == (CPU_K6)))) && (which_alternative == 0))
        {
	  return 0;
        }
      else
        {
	  return 1;
        }

    case 1035:
    case 1034:
    case 1033:
    case 1032:
    case 1031:
    case 1030:
    case 1029:
    case 1028:
    case 1027:
    case 1026:
    case 1025:
    case 1022:
    case 1021:
    case 1020:
    case 1019:
    case 1018:
    case 1017:
    case 1016:
    case 1015:
    case 1014:
    case 1013:
    case 1012:
    case 1011:
    case 1010:
    case 1009:
    case 1008:
    case 1007:
    case 1006:
    case 1005:
    case 1004:
    case 1003:
    case 1002:
    case 1001:
    case 1000:
    case 999:
    case 998:
    case 997:
    case 996:
    case 995:
    case 994:
    case 993:
    case 992:
    case 991:
    case 990:
    case 989:
    case 988:
    case 987:
    case 986:
    case 985:
    case 984:
    case 983:
    case 982:
    case 981:
    case 980:
    case 979:
    case 978:
    case 977:
    case 976:
    case 975:
    case 974:
    case 973:
    case 972:
    case 971:
    case 970:
    case 969:
    case 968:
    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 961:
    case 960:
    case 959:
    case 958:
    case 957:
    case 956:
    case 955:
    case 954:
    case 953:
    case 952:
    case 951:
    case 950:
    case 949:
    case 948:
    case 947:
    case 946:
    case 945:
    case 944:
    case 943:
    case 942:
    case 941:
    case 940:
    case 939:
    case 938:
    case 937:
    case 936:
    case 935:
    case 934:
    case 933:
    case 932:
    case 931:
    case 930:
    case 929:
    case 928:
    case 927:
    case 926:
    case 925:
    case 924:
    case 923:
    case 922:
    case 921:
    case 920:
    case 919:
    case 918:
    case 917:
    case 916:
    case 915:
    case 914:
    case 913:
    case 912:
    case 911:
    case 910:
    case 909:
    case 908:
    case 907:
    case 906:
    case 905:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 898:
    case 897:
    case 896:
    case 895:
    case 894:
    case 893:
    case 892:
    case 891:
    case 890:
    case 889:
    case 888:
    case 887:
    case 886:
    case 885:
    case 884:
    case 883:
    case 882:
    case 881:
    case 880:
    case 879:
    case 878:
    case 877:
    case 876:
    case 875:
    case 874:
    case 873:
    case 872:
    case 871:
    case 870:
    case 869:
    case 868:
    case 867:
    case 866:
    case 865:
    case 864:
    case 863:
    case 862:
    case 861:
    case 860:
    case 859:
    case 858:
    case 857:
    case 856:
    case 855:
    case 853:
    case 852:
    case 851:
    case 850:
    case 849:
    case 848:
    case 847:
    case 846:
    case 845:
    case 844:
    case 843:
    case 842:
    case 841:
    case 840:
    case 839:
    case 838:
    case 837:
    case 836:
    case 835:
    case 834:
    case 833:
    case 832:
    case 831:
    case 830:
    case 829:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 822:
    case 821:
    case 820:
    case 819:
    case 818:
    case 817:
    case 816:
    case 815:
    case 814:
    case 813:
    case 812:
    case 811:
    case 810:
    case 809:
    case 808:
    case 807:
    case 806:
    case 805:
    case 804:
    case 803:
    case 802:
    case 801:
    case 800:
    case 799:
    case 798:
    case 797:
    case 796:
    case 795:
    case 794:
    case 793:
    case 792:
    case 791:
    case 790:
    case 789:
    case 788:
    case 787:
    case 786:
    case 785:
    case 784:
    case 783:
    case 782:
    case 781:
    case 780:
    case 779:
    case 778:
    case 777:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 770:
    case 769:
    case 768:
    case 767:
    case 766:
    case 765:
    case 764:
    case 763:
    case 762:
    case 761:
    case 760:
    case 759:
    case 758:
    case 757:
    case 756:
    case 755:
    case 754:
    case 753:
    case 752:
    case 751:
    case 750:
    case 749:
    case 748:
    case 747:
    case 746:
    case 745:
    case 744:
    case 743:
    case 742:
    case 741:
    case 740:
    case 739:
    case 738:
    case 737:
    case 736:
    case 735:
    case 734:
    case 733:
    case 732:
    case 731:
    case 730:
    case 729:
    case 728:
    case 727:
    case 726:
    case 725:
    case 724:
    case 723:
    case 722:
    case 721:
    case 720:
    case 719:
    case 718:
    case 717:
    case 716:
    case 715:
    case 713:
    case 712:
    case 711:
    case 710:
    case 709:
    case 691:
    case 690:
    case 689:
    case 688:
    case 687:
    case 686:
    case 685:
    case 684:
    case 683:
    case 682:
    case 657:
    case 654:
    case 651:
    case 648:
    case 640:
    case 639:
    case 637:
    case 586:
    case 583:
    case 553:
    case 550:
    case 502:
    case 501:
    case 500:
    case 499:
    case 172:
    case 169:
    case 166:
    case 163:
    case 155:
    case 154:
    case 150:
    case 149:
    case 137:
    case 128:
    case 126:
    case 125:
    case 121:
    case 120:
    case 117:
    case 116:
    case 113:
    case 110:
    case 107:
    case 69:
    case 64:
    case 63:
    case 36:
    case 33:
    case 412:
    case 430:
    case 536:
      return 1;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int get_attr_prefix_rep PARAMS ((rtx));
int
get_attr_prefix_rep (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 563:
    case 562:
    case 558:
    case 557:
      if (get_attr_unit (insn) == UNIT_SSE)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 3))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative != 0) && ((which_alternative != 3) && (which_alternative != 4)))) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 1035:
    case 1034:
    case 1018:
    case 1017:
    case 1016:
    case 928:
    case 927:
    case 926:
    case 925:
    case 904:
    case 903:
    case 902:
    case 901:
    case 898:
    case 896:
    case 894:
    case 892:
    case 890:
    case 888:
    case 886:
    case 791:
    case 790:
    case 789:
    case 788:
    case 787:
    case 786:
    case 785:
    case 782:
    case 780:
    case 776:
    case 775:
    case 774:
    case 773:
    case 744:
    case 742:
    case 740:
    case 738:
    case 736:
    case 734:
    case 732:
    case 729:
    case 728:
    case 727:
    case 657:
    case 654:
    case 651:
    case 648:
    case 586:
    case 583:
    case 553:
    case 550:
    case 502:
    case 501:
    case 172:
    case 169:
    case 166:
    case 163:
    case 137:
    case 128:
    case 613:
    case 614:
    case 615:
    case 616:
    case 617:
    case 625:
    case 626:
    case 627:
    case 628:
    case 629:
    case 630:
    case 631:
    case 632:
    case 633:
    case 634:
    case 635:
      return 1;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int get_attr_prefix_data16 PARAMS ((rtx));
int
get_attr_prefix_data16 (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 417:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 292:
    case 209:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 50:
      if (get_attr_mode (insn) == MODE_HI)
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 1030:
    case 1028:
    case 1026:
    case 1019:
    case 1015:
    case 1014:
    case 1004:
    case 1003:
    case 930:
    case 915:
    case 912:
    case 911:
    case 909:
    case 906:
    case 905:
    case 900:
    case 899:
    case 897:
    case 895:
    case 893:
    case 891:
    case 889:
    case 887:
    case 885:
    case 760:
    case 759:
    case 758:
    case 757:
    case 756:
    case 755:
    case 754:
    case 753:
    case 689:
    case 640:
    case 622:
    case 621:
    case 610:
    case 609:
    case 494:
    case 482:
    case 468:
    case 466:
    case 444:
    case 442:
    case 419:
    case 418:
    case 405:
    case 404:
    case 357:
    case 356:
    case 337:
    case 336:
    case 335:
    case 315:
    case 314:
    case 313:
    case 293:
    case 278:
    case 275:
    case 248:
    case 240:
    case 239:
    case 238:
    case 214:
    case 212:
    case 211:
    case 210:
    case 160:
    case 159:
    case 124:
    case 110:
    case 109:
    case 108:
    case 57:
    case 56:
    case 55:
    case 53:
    case 52:
    case 51:
    case 48:
    case 8:
    case 7:
    case 6:
      return 1;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern enum attr_type get_attr_type PARAMS ((rtx));
enum attr_type
get_attr_type (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 714:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_OTHER;
        }
      else
        {
	  return TYPE_SSEMOV;
        }

    case 642:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_FCMOV;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_MULTI;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_FCMOV;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_ICMOV;
        }
      else
        {
	  return TYPE_ICMOV;
        }

    case 292:
    case 288:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_IMOVX;
        }

    case 286:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_IMOVX;
        }

    case 202:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  return TYPE_LEA;
        }
      else if (incdec_operand (operands[2], SImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU;
        }

    case 201:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  return TYPE_LEA;
        }
      else if (incdec_operand (operands[2], SImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU;
        }

    case 196:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))
        {
	  return TYPE_LEA;
        }
      else if (incdec_operand (operands[2], DImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_FMOV;
        }
      else if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return TYPE_MULTI;
        }
      else
        {
	  return TYPE_SSECVT;
        }

    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_FMOV;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_MULTI;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_FMOV;
        }
      else
        {
	  return TYPE_SSECVT;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_FMOV;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_MULTI;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_FMOV;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_MULTI;
        }
      else
        {
	  return TYPE_SSEMOV;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_FMOV;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_IMOV;
        }
      else if ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return TYPE_SSEMOV;
        }
      else
        {
	  return TYPE_MMXMOV;
        }

    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_OTHER;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_MMX;
        }
      else
        {
	  return TYPE_SSEMOV;
        }

    case 59:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0))))
        {
	  return TYPE_IMOV;
        }
      else if ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 50:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_HIMODE_MATH) == (0)))) || (((which_alternative == 1) || (which_alternative == 2)) && (aligned_operand (operands[1], HImode))))
        {
	  return TYPE_IMOV;
        }
      else if (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 0:
    case 3:
    case 6:
    case 9:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_TEST;
        }
      else
        {
	  return TYPE_ICMP;
        }

    case 32:
    case 35:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_FCMP;
        }
      else
        {
	  return TYPE_SSECMP;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return TYPE_MMXMOV;
        }
      else if ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return TYPE_SSEMOV;
        }
      else if (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))
        {
	  return TYPE_LEA;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 65:
      extract_constrain_insn_cached (insn);
      if ((register_operand (operands[0], QImode)) && ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))))
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 66:
      extract_constrain_insn_cached (insn);
      if ((register_operand (operands[0], QImode)) && ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))))
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 70:
      extract_constrain_insn_cached (insn);
      if ((register_operand (operands[0], QImode)) && ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))))
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 71:
      extract_constrain_insn_cached (insn);
      if ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_PUSH;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return TYPE_MMXMOV;
        }
      else if ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))
        {
	  return TYPE_SSEMOV;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_MULTI;
        }
      else if (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))
        {
	  return TYPE_LEA;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 87:
    case 88:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MULTI;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_PUSH;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 109:
    case 112:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_ALU1;
        }

    case 115:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_IMOV;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_SSECVT;
        }
      else
        {
	  return TYPE_FMOV;
        }

    case 162:
    case 165:
    case 168:
    case 171:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_FMOV;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_MULTI;
        }
      else
        {
	  return TYPE_SSECVT;
        }

    case 161:
    case 164:
    case 167:
    case 170:
    case 173:
    case 174:
    case 175:
    case 176:
    case 177:
    case 178:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_FMOV;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 197:
    case 198:
    case 199:
    case 200:
      extract_insn_cached (insn);
      if (incdec_operand (operands[2], DImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU;
        }

    case 203:
    case 204:
    case 205:
    case 206:
    case 207:
    case 208:
      extract_insn_cached (insn);
      if (incdec_operand (operands[2], SImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU;
        }

    case 209:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  return TYPE_LEA;
        }
      else
        {
	  if (incdec_operand (operands[2], HImode))
	    {
	      return TYPE_INCDEC;
	    }
	  else
	    {
	      return TYPE_ALU;
	    }
        }

    case 215:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 3)
        {
	  return TYPE_LEA;
        }
      else
        {
	  if (incdec_operand (operands[2], QImode))
	    {
	      return TYPE_INCDEC;
	    }
	  else
	    {
	      return TYPE_ALU;
	    }
        }

    case 217:
      extract_insn_cached (insn);
      if (incdec_operand (operands[2], QImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU1;
        }

    case 210:
    case 211:
    case 212:
    case 213:
    case 214:
    case 220:
      extract_insn_cached (insn);
      if (incdec_operand (operands[2], HImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU;
        }

    case 216:
    case 218:
    case 219:
    case 221:
    case 222:
    case 223:
      extract_insn_cached (insn);
      if (incdec_operand (operands[2], QImode))
        {
	  return TYPE_INCDEC;
        }
      else
        {
	  return TYPE_ALU;
        }

    case 408:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return TYPE_LEA;
        }
      else if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 409:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 413:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return TYPE_LEA;
        }
      else if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 414:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return TYPE_LEA;
        }
      else if (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 415:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 416:
      extract_constrain_insn_cached (insn);
      if (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 417:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  return TYPE_LEA;
        }
      else if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 418:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 419:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 420:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  return TYPE_LEA;
        }
      else if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 421:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 422:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (register_operand (operands[0], VOIDmode))) && (const1_operand (operands[2], VOIDmode)))
        {
	  return TYPE_ALU;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 423:
    case 431:
    case 432:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_IMOVX;
        }
      else
        {
	  return TYPE_ISHIFT;
        }

    case 548:
    case 551:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], SFmode))
        {
	  return TYPE_FMUL;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 549:
    case 552:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  if (mult_operator (operands[3], SFmode))
	    {
	      return TYPE_SSEMUL;
	    }
	  else
	    {
	      return TYPE_SSEADD;
	    }
        }
      else
        {
	  if (mult_operator (operands[3], SFmode))
	    {
	      return TYPE_FMUL;
	    }
	  else
	    {
	      return TYPE_FOP;
	    }
        }

    case 550:
    case 553:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], SFmode))
        {
	  return TYPE_SSEMUL;
        }
      else
        {
	  return TYPE_SSEADD;
        }

    case 554:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], XFmode))
        {
	  return TYPE_FMUL;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 555:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], TFmode))
        {
	  return TYPE_FMUL;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (mult_operator (operands[3], SFmode)))
        {
	  return TYPE_SSEMUL;
        }
      else if ((which_alternative == 2) && (div_operator (operands[3], SFmode)))
        {
	  return TYPE_SSEDIV;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_SSEADD;
        }
      else if (mult_operator (operands[3], SFmode))
        {
	  return TYPE_FMUL;
        }
      else if (div_operator (operands[3], SFmode))
        {
	  return TYPE_FDIV;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 556:
    case 559:
    case 560:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], SFmode))
        {
	  return TYPE_FMUL;
        }
      else if (div_operator (operands[3], SFmode))
        {
	  return TYPE_FDIV;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (mult_operator (operands[3], SFmode)))
        {
	  return TYPE_SSEMUL;
        }
      else if ((which_alternative == 2) && (div_operator (operands[3], SFmode)))
        {
	  return TYPE_SSEDIV;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_SSEADD;
        }
      else if (mult_operator (operands[3], DFmode))
        {
	  return TYPE_FMUL;
        }
      else if (div_operator (operands[3], DFmode))
        {
	  return TYPE_FDIV;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 558:
    case 563:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], SFmode))
        {
	  return TYPE_SSEMUL;
        }
      else if (div_operator (operands[3], SFmode))
        {
	  return TYPE_SSEDIV;
        }
      else
        {
	  return TYPE_SSEADD;
        }

    case 561:
    case 564:
    case 565:
    case 566:
    case 567:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], DFmode))
        {
	  return TYPE_FMUL;
        }
      else if (div_operator (operands[3], DFmode))
        {
	  return TYPE_FDIV;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 568:
    case 570:
    case 572:
    case 574:
    case 576:
    case 578:
    case 580:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], XFmode))
        {
	  return TYPE_FMUL;
        }
      else if (div_operator (operands[3], XFmode))
        {
	  return TYPE_FDIV;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 569:
    case 571:
    case 573:
    case 575:
    case 577:
    case 579:
    case 581:
      extract_insn_cached (insn);
      if (mult_operator (operands[3], TFmode))
        {
	  return TYPE_FMUL;
        }
      else if (div_operator (operands[3], TFmode))
        {
	  return TYPE_FDIV;
        }
      else
        {
	  return TYPE_FOP;
        }

    case 582:
    case 585:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_FPSPC;
        }
      else
        {
	  return TYPE_SSE;
        }

    case 658:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_ALU;
        }
      else if (const0_operand (operands[2], SImode))
        {
	  return TYPE_IMOV;
        }
      else
        {
	  return TYPE_LEA;
        }

    case 659:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_ALU;
        }
      else if (const0_operand (operands[2], DImode))
        {
	  return TYPE_IMOV;
        }
      else
        {
	  return TYPE_LEA;
        }

    case 1009:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_SSECVT;
        }
      else
        {
	  return TYPE_SSEMOV;
        }

    case 1010:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_SSECVT;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_SSEMOV;
        }
      else
        {
	  return TYPE_SSECVT;
        }

    case 817:
    case 818:
    case 819:
    case 833:
    case 834:
    case 835:
    case 836:
    case 837:
    case 838:
    case 839:
    case 840:
    case 841:
    case 842:
    case 843:
    case 872:
      return TYPE_MMXSHFT;

    case 687:
    case 688:
    case 719:
    case 720:
    case 820:
    case 821:
    case 822:
    case 844:
    case 845:
    case 846:
    case 847:
    case 848:
    case 849:
    case 865:
    case 866:
    case 870:
    case 871:
    case 879:
    case 880:
      return TYPE_MMXCVT;

    case 823:
    case 824:
    case 825:
    case 826:
    case 827:
    case 828:
    case 858:
    case 859:
    case 860:
      return TYPE_MMXCMP;

    case 808:
    case 809:
    case 810:
    case 811:
    case 863:
    case 878:
      return TYPE_MMXMUL;

    case 792:
    case 793:
    case 794:
    case 795:
    case 796:
    case 797:
    case 798:
    case 799:
    case 800:
    case 801:
    case 802:
    case 803:
    case 804:
    case 805:
    case 806:
    case 807:
    case 812:
    case 813:
    case 814:
    case 815:
    case 816:
    case 829:
    case 830:
    case 831:
    case 832:
    case 855:
    case 856:
    case 857:
    case 861:
    case 862:
    case 867:
    case 868:
    case 869:
      return TYPE_MMXADD;

    case 685:
    case 686:
    case 722:
      return TYPE_MMXMOV;

    case 709:
    case 710:
    case 711:
    case 712:
    case 850:
    case 864:
    case 873:
    case 874:
    case 875:
    case 876:
    case 877:
    case 883:
    case 884:
      return TYPE_MMX;

    case 737:
    case 738:
    case 891:
    case 892:
      return TYPE_SSEDIV;

    case 128:
    case 137:
    case 149:
    case 150:
    case 154:
    case 155:
    case 163:
    case 166:
    case 169:
    case 172:
    case 716:
    case 717:
    case 718:
    case 723:
    case 724:
    case 725:
    case 726:
    case 730:
    case 777:
    case 778:
    case 783:
    case 784:
    case 785:
    case 786:
    case 787:
    case 788:
    case 789:
    case 790:
    case 791:
    case 905:
    case 906:
    case 907:
    case 908:
    case 909:
    case 910:
    case 911:
    case 912:
    case 913:
    case 914:
    case 915:
    case 916:
    case 917:
    case 918:
    case 919:
    case 920:
    case 921:
    case 922:
    case 923:
    case 924:
    case 925:
    case 926:
    case 927:
    case 928:
    case 929:
    case 930:
    case 957:
    case 958:
    case 959:
    case 960:
    case 961:
    case 990:
    case 991:
    case 992:
    case 993:
    case 994:
    case 995:
    case 996:
    case 997:
    case 998:
    case 999:
    case 1000:
    case 1001:
    case 1002:
    case 1004:
    case 1006:
    case 1007:
    case 1008:
    case 1014:
    case 1015:
    case 1016:
    case 1017:
    case 1018:
    case 1019:
    case 1033:
    case 1034:
    case 1035:
      return TYPE_SSECVT;

    case 33:
    case 36:
    case 501:
    case 502:
    case 771:
    case 772:
    case 773:
    case 774:
    case 775:
    case 776:
    case 899:
    case 900:
    case 901:
    case 902:
    case 903:
    case 904:
    case 962:
    case 963:
    case 964:
    case 965:
    case 966:
    case 967:
      return TYPE_SSECMP;

    case 735:
    case 736:
    case 889:
    case 890:
      return TYPE_SSEMUL;

    case 731:
    case 732:
    case 733:
    case 734:
    case 885:
    case 886:
    case 887:
    case 888:
    case 893:
    case 894:
    case 895:
    case 896:
    case 1025:
    case 1026:
    case 1027:
    case 1028:
    case 1029:
    case 1030:
      return TYPE_SSEADD;

    case 682:
    case 683:
    case 684:
    case 689:
    case 690:
    case 691:
    case 713:
    case 715:
    case 721:
    case 727:
    case 728:
    case 729:
    case 1003:
    case 1005:
    case 1011:
    case 1012:
    case 1013:
      return TYPE_SSEMOV;

    case 583:
    case 586:
    case 648:
    case 651:
    case 654:
    case 657:
    case 739:
    case 740:
    case 741:
    case 742:
    case 743:
    case 744:
    case 779:
    case 780:
    case 781:
    case 782:
    case 851:
    case 852:
    case 853:
    case 881:
    case 882:
    case 897:
    case 898:
    case 1020:
    case 1021:
    case 1022:
    case 1031:
    case 1032:
      return TYPE_SSE;

    case 947:
    case 948:
    case 949:
    case 950:
    case 951:
      return TYPE_SSEIMUL;

    case 972:
    case 973:
    case 974:
    case 975:
    case 976:
    case 977:
    case 978:
    case 979:
    case 980:
    case 981:
    case 982:
    case 983:
    case 984:
    case 985:
    case 986:
    case 987:
    case 988:
    case 989:
      return TYPE_SSEISHFT;

    case 931:
    case 932:
    case 933:
    case 934:
    case 935:
    case 936:
    case 937:
    case 938:
    case 939:
    case 940:
    case 941:
    case 942:
    case 943:
    case 944:
    case 945:
    case 946:
    case 952:
    case 953:
    case 954:
    case 955:
    case 956:
    case 968:
    case 969:
    case 970:
    case 971:
      return TYPE_SSEIADD;

    case 745:
    case 746:
    case 747:
    case 748:
    case 749:
    case 750:
    case 751:
    case 752:
    case 753:
    case 754:
    case 755:
    case 756:
    case 757:
    case 758:
    case 759:
    case 760:
    case 761:
    case 762:
    case 763:
    case 764:
    case 765:
    case 766:
    case 767:
    case 768:
    case 769:
    case 770:
      return TYPE_SSELOG;

    case 146:
    case 147:
    case 148:
    case 151:
    case 152:
    case 153:
    case 156:
    case 157:
    case 158:
      return TYPE_FISTP;

    case 90:
    case 95:
    case 104:
    case 105:
      return TYPE_FXCH;

    case 19:
    case 20:
    case 21:
    case 23:
    case 24:
    case 27:
    case 31:
    case 34:
      return TYPE_FCMP;

    case 644:
    case 645:
      return TYPE_FCMOV;

    case 584:
    case 587:
    case 588:
    case 589:
    case 590:
    case 591:
    case 592:
    case 593:
    case 594:
    case 595:
    case 596:
    case 597:
    case 598:
    case 599:
    case 600:
    case 601:
    case 602:
    case 603:
    case 604:
      return TYPE_FPSPC;

    case 370:
    case 371:
    case 372:
    case 373:
    case 374:
    case 375:
    case 376:
    case 377:
    case 378:
    case 389:
    case 390:
    case 391:
    case 392:
    case 393:
    case 394:
    case 395:
    case 396:
    case 397:
      return TYPE_FSGN;

    case 129:
    case 130:
    case 131:
    case 132:
    case 136:
    case 139:
    case 141:
    case 143:
    case 145:
      return TYPE_FMOV;

    case 605:
      return TYPE_CLD;

    case 606:
    case 607:
    case 608:
    case 609:
    case 610:
    case 611:
    case 612:
    case 613:
    case 614:
    case 615:
    case 616:
    case 617:
    case 618:
    case 619:
    case 620:
    case 621:
    case 622:
    case 623:
    case 624:
    case 625:
    case 626:
    case 627:
    case 628:
    case 629:
    case 630:
    case 631:
    case 632:
    case 633:
    case 634:
    case 635:
      return TYPE_STR;

    case 674:
    case 675:
    case 676:
    case 677:
    case 678:
    case 679:
      return TYPE_CALLV;

    case 521:
    case 522:
    case 523:
    case 524:
    case 525:
      return TYPE_CALL;

    case 40:
    case 41:
    case 78:
    case 79:
      return TYPE_POP;

    case 37:
    case 38:
    case 39:
    case 48:
    case 49:
    case 57:
    case 58:
    case 77:
      return TYPE_PUSH;

    case 637:
    case 639:
    case 640:
      return TYPE_ICMOV;

    case 499:
    case 500:
      return TYPE_SETCC;

    case 503:
    case 504:
    case 515:
    case 516:
    case 517:
    case 518:
    case 519:
    case 520:
    case 529:
      return TYPE_IBR;

    case 14:
    case 276:
    case 277:
    case 278:
    case 279:
    case 280:
    case 281:
    case 282:
    case 283:
      return TYPE_TEST;

    case 1:
    case 2:
    case 4:
    case 5:
    case 7:
    case 8:
    case 10:
    case 11:
    case 12:
    case 13:
    case 15:
    case 16:
    case 17:
      return TYPE_ICMP;

    case 262:
    case 263:
    case 266:
    case 269:
    case 272:
    case 274:
    case 275:
      return TYPE_IDIV;

    case 245:
    case 246:
    case 247:
    case 248:
    case 249:
    case 250:
    case 251:
    case 252:
    case 253:
    case 254:
    case 255:
    case 256:
    case 257:
    case 258:
    case 259:
    case 260:
    case 261:
      return TYPE_IMUL;

    case 483:
    case 485:
    case 496:
    case 498:
      return TYPE_ROTATE1;

    case 475:
    case 476:
    case 477:
    case 478:
    case 479:
    case 480:
    case 481:
    case 482:
    case 484:
    case 486:
    case 487:
    case 488:
    case 489:
    case 490:
    case 491:
    case 492:
    case 493:
    case 494:
    case 495:
    case 497:
      return TYPE_ROTATE;

    case 446:
    case 448:
    case 470:
    case 472:
      return TYPE_ISHIFT1;

    case 412:
    case 424:
    case 425:
    case 426:
    case 427:
    case 430:
    case 433:
    case 434:
    case 435:
    case 436:
    case 437:
    case 438:
    case 439:
    case 440:
    case 441:
    case 442:
    case 443:
    case 444:
    case 445:
    case 447:
    case 449:
    case 450:
    case 451:
    case 452:
    case 453:
    case 454:
    case 457:
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
    case 471:
    case 473:
    case 474:
      return TYPE_ISHIFT;

    case 186:
    case 187:
    case 188:
    case 189:
    case 190:
    case 191:
    case 192:
    case 193:
    case 194:
    case 195:
      return TYPE_LEA;

    case 63:
    case 64:
    case 69:
    case 107:
    case 110:
    case 113:
    case 116:
    case 117:
    case 119:
    case 120:
    case 121:
    case 122:
    case 123:
    case 124:
    case 125:
    case 126:
      return TYPE_IMOVX;

    case 45:
    case 46:
    case 47:
    case 51:
    case 52:
    case 53:
    case 54:
    case 55:
    case 60:
    case 61:
    case 67:
    case 68:
    case 72:
    case 73:
    case 74:
    case 84:
    case 85:
    case 86:
    case 544:
    case 545:
      return TYPE_IMOV;

    case 350:
    case 351:
    case 352:
    case 353:
    case 354:
    case 355:
    case 356:
    case 357:
    case 358:
    case 359:
    case 398:
    case 400:
    case 401:
    case 404:
    case 406:
      return TYPE_NEGNOT;

    case 42:
    case 43:
    case 56:
    case 62:
    case 80:
    case 81:
    case 106:
    case 108:
    case 111:
    case 242:
    case 295:
    case 297:
    case 317:
    case 319:
    case 339:
    case 345:
    case 399:
    case 402:
    case 403:
    case 405:
    case 407:
      return TYPE_ALU1;

    case 180:
    case 181:
    case 182:
    case 183:
    case 184:
    case 185:
    case 224:
    case 226:
    case 227:
    case 228:
    case 229:
    case 230:
    case 231:
    case 232:
    case 233:
    case 234:
    case 235:
    case 236:
    case 237:
    case 238:
    case 239:
    case 240:
    case 241:
    case 243:
    case 244:
    case 287:
    case 289:
    case 290:
    case 291:
    case 293:
    case 294:
    case 296:
    case 298:
    case 299:
    case 300:
    case 301:
    case 302:
    case 303:
    case 304:
    case 305:
    case 306:
    case 307:
    case 308:
    case 309:
    case 310:
    case 311:
    case 312:
    case 313:
    case 314:
    case 315:
    case 316:
    case 318:
    case 320:
    case 321:
    case 322:
    case 323:
    case 324:
    case 325:
    case 326:
    case 327:
    case 328:
    case 329:
    case 330:
    case 331:
    case 332:
    case 333:
    case 334:
    case 335:
    case 336:
    case 337:
    case 338:
    case 340:
    case 341:
    case 342:
    case 343:
    case 344:
    case 346:
    case 347:
    case 348:
    case 546:
    case 547:
    case 636:
    case 638:
      return TYPE_ALU;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 18:
    case 22:
    case 25:
    case 26:
    case 28:
    case 91:
    case 92:
    case 96:
    case 97:
    case 98:
    case 99:
    case 264:
    case 265:
    case 267:
    case 268:
    case 270:
    case 271:
    case 273:
    case 410:
    case 411:
    case 428:
    case 429:
    case 455:
    case 456:
    case 531:
    case 537:
    case 538:
    case 539:
    case 540:
    case 541:
    case 542:
    case 672:
    case 673:
    case 702:
    case 703:
    case 704:
    case 705:
    case 706:
    case 707:
    case 708:
      return TYPE_MULTI;

    default:
      return TYPE_OTHER;

    }
}

extern enum attr_unit get_attr_unit PARAMS ((rtx));
enum attr_unit
get_attr_unit (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 714:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (which_alternative != 1))
        {
	  return UNIT_SSE;
        }
      else
        {
	  return UNIT_UNKNOWN;
        }

    case 643:
    case 642:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FOP) || ((mult_operator (operands[3], TFmode)) || (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FOP) || ((mult_operator (operands[3], XFmode)) || (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 562:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FOP) || (((which_alternative != 2) && (mult_operator (operands[3], DFmode))) || (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return UNIT_I387;
        }
      else if ((get_attr_type (insn) == TYPE_SSEADD) || (((which_alternative == 2) && (mult_operator (operands[3], SFmode))) || (get_attr_type (insn) == TYPE_SSEDIV)))
        {
	  return UNIT_SSE;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FOP) || ((mult_operator (operands[3], DFmode)) || (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 563:
    case 558:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_SSEADD) || ((mult_operator (operands[3], SFmode)) || (get_attr_type (insn) == TYPE_SSEDIV)))
        {
	  return UNIT_SSE;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 557:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FOP) || (((which_alternative != 2) && (mult_operator (operands[3], SFmode))) || (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return UNIT_I387;
        }
      else if ((get_attr_type (insn) == TYPE_SSEADD) || (((which_alternative == 2) && (mult_operator (operands[3], SFmode))) || (get_attr_type (insn) == TYPE_SSEDIV)))
        {
	  return UNIT_SSE;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_FOP) || ((mult_operator (operands[3], SFmode)) || (get_attr_type (insn) == TYPE_FDIV)))
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_SSE;
        }

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return UNIT_I387;
        }
      else if (which_alternative != 1)
        {
	  return UNIT_SSE;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_SSE;
        }

    case 134:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return UNIT_I387;
        }
      else if ((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 3)))
        {
	  return UNIT_SSE;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_SSE;
        }

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return UNIT_I387;
        }
      else if ((which_alternative != 3) && (which_alternative != 4))
        {
	  return UNIT_SSE;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return UNIT_I387;
        }
      else if ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return UNIT_SSE;
        }
      else if ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))
        {
	  return UNIT_MMX;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return UNIT_SSE;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return UNIT_MMX;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 3))))
        {
	  return UNIT_SSE;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return UNIT_MMX;
        }
      else if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return UNIT_UNKNOWN;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return UNIT_SSE;
        }
      else if ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return UNIT_MMX;
        }
      else
        {
	  return UNIT_INTEGER;
        }

    case 585:
    case 582:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return UNIT_I387;
        }
      else
        {
	  return UNIT_SSE;
        }

    case 1024:
    case 1023:
    case 854:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 543:
    case 536:
    case 535:
    case 534:
    case 533:
    case 532:
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 225:
    case 179:
    case 118:
    case 114:
    case 75:
    case 30:
      return UNIT_UNKNOWN;

    case 884:
    case 883:
    case 880:
    case 879:
    case 878:
    case 877:
    case 876:
    case 875:
    case 874:
    case 873:
    case 872:
    case 871:
    case 870:
    case 869:
    case 868:
    case 867:
    case 866:
    case 865:
    case 864:
    case 863:
    case 862:
    case 861:
    case 860:
    case 859:
    case 858:
    case 857:
    case 856:
    case 855:
    case 850:
    case 849:
    case 848:
    case 847:
    case 846:
    case 845:
    case 844:
    case 843:
    case 842:
    case 841:
    case 840:
    case 839:
    case 838:
    case 837:
    case 836:
    case 835:
    case 834:
    case 833:
    case 832:
    case 831:
    case 830:
    case 829:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 822:
    case 821:
    case 820:
    case 819:
    case 818:
    case 817:
    case 816:
    case 815:
    case 814:
    case 813:
    case 812:
    case 811:
    case 810:
    case 809:
    case 808:
    case 807:
    case 806:
    case 805:
    case 804:
    case 803:
    case 802:
    case 801:
    case 800:
    case 799:
    case 798:
    case 797:
    case 796:
    case 795:
    case 794:
    case 793:
    case 792:
    case 722:
    case 720:
    case 719:
    case 712:
    case 711:
    case 710:
    case 709:
    case 688:
    case 687:
    case 686:
    case 685:
      return UNIT_MMX;

    case 1035:
    case 1034:
    case 1033:
    case 1032:
    case 1031:
    case 1030:
    case 1029:
    case 1028:
    case 1027:
    case 1026:
    case 1025:
    case 1022:
    case 1021:
    case 1020:
    case 1019:
    case 1018:
    case 1017:
    case 1016:
    case 1015:
    case 1014:
    case 1013:
    case 1012:
    case 1011:
    case 1010:
    case 1009:
    case 1008:
    case 1007:
    case 1006:
    case 1005:
    case 1004:
    case 1003:
    case 1002:
    case 1001:
    case 1000:
    case 999:
    case 998:
    case 997:
    case 996:
    case 995:
    case 994:
    case 993:
    case 992:
    case 991:
    case 990:
    case 989:
    case 988:
    case 987:
    case 986:
    case 985:
    case 984:
    case 983:
    case 982:
    case 981:
    case 980:
    case 979:
    case 978:
    case 977:
    case 976:
    case 975:
    case 974:
    case 973:
    case 972:
    case 971:
    case 970:
    case 969:
    case 968:
    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 961:
    case 960:
    case 959:
    case 958:
    case 957:
    case 956:
    case 955:
    case 954:
    case 953:
    case 952:
    case 951:
    case 950:
    case 949:
    case 948:
    case 947:
    case 946:
    case 945:
    case 944:
    case 943:
    case 942:
    case 941:
    case 940:
    case 939:
    case 938:
    case 937:
    case 936:
    case 935:
    case 934:
    case 933:
    case 932:
    case 931:
    case 930:
    case 929:
    case 928:
    case 927:
    case 926:
    case 925:
    case 924:
    case 923:
    case 922:
    case 921:
    case 920:
    case 919:
    case 918:
    case 917:
    case 916:
    case 915:
    case 914:
    case 913:
    case 912:
    case 911:
    case 910:
    case 909:
    case 908:
    case 907:
    case 906:
    case 905:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 898:
    case 897:
    case 896:
    case 895:
    case 894:
    case 893:
    case 892:
    case 891:
    case 890:
    case 889:
    case 888:
    case 887:
    case 886:
    case 885:
    case 882:
    case 881:
    case 853:
    case 852:
    case 851:
    case 791:
    case 790:
    case 789:
    case 788:
    case 787:
    case 786:
    case 785:
    case 784:
    case 783:
    case 782:
    case 781:
    case 780:
    case 779:
    case 778:
    case 777:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 770:
    case 769:
    case 768:
    case 767:
    case 766:
    case 765:
    case 764:
    case 763:
    case 762:
    case 761:
    case 760:
    case 759:
    case 758:
    case 757:
    case 756:
    case 755:
    case 754:
    case 753:
    case 752:
    case 751:
    case 750:
    case 749:
    case 748:
    case 747:
    case 746:
    case 745:
    case 744:
    case 743:
    case 742:
    case 741:
    case 740:
    case 739:
    case 738:
    case 737:
    case 736:
    case 735:
    case 734:
    case 733:
    case 732:
    case 731:
    case 730:
    case 729:
    case 728:
    case 727:
    case 726:
    case 725:
    case 724:
    case 723:
    case 721:
    case 718:
    case 717:
    case 716:
    case 715:
    case 713:
    case 691:
    case 690:
    case 689:
    case 684:
    case 683:
    case 682:
    case 657:
    case 654:
    case 651:
    case 648:
    case 586:
    case 583:
    case 553:
    case 550:
    case 502:
    case 501:
    case 172:
    case 169:
    case 166:
    case 163:
    case 155:
    case 154:
    case 150:
    case 149:
    case 137:
    case 128:
    case 36:
    case 33:
      return UNIT_SSE;

    case 645:
    case 644:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
    case 555:
    case 554:
    case 551:
    case 548:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 105:
    case 104:
    case 95:
    case 90:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
    case 29:
    case 159:
    case 160:
      return UNIT_I387;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return UNIT_INTEGER;

    }
}

static int athlon_muldiv_unit_blockage PARAMS ((rtx, rtx));
static int
athlon_muldiv_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 1;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 42 /* 0x2a */;

    default:
      abort ();
    }
}

static int athlon_muldiv_unit_conflict_cost PARAMS ((rtx, rtx));
static int
athlon_muldiv_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 1;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 42 /* 0x2a */;

    default:
      abort ();
    }
}

static int athlon_ieu_unit_blockage PARAMS ((rtx, rtx));
static int
athlon_ieu_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      casenum = 1;
      break;

    case 420:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((which_alternative == 2) || (get_attr_type (insn) == TYPE_ISHIFT)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) || ((which_alternative != 0) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if ((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_ISHIFT))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ISHIFT)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 2;
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) || ((which_alternative == 3) || (incdec_operand (operands[2], QImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) || ((which_alternative == 2) || (incdec_operand (operands[2], HImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode)))) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 71:
      extract_constrain_insn_cached (insn);
      if (((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 70:
    case 66:
    case 65:
      if ((get_attr_type (insn) == TYPE_IMOV) || (get_attr_type (insn) == TYPE_IMOVX))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 50:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMOV) || ((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
    case 659:
    case 658:
    case 640:
    case 639:
    case 638:
    case 637:
    case 636:
    case 605:
    case 547:
    case 546:
    case 545:
    case 544:
    case 529:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 423:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 294:
    case 293:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 242:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 115:
    case 113:
    case 112:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 86:
    case 85:
    case 84:
    case 81:
    case 80:
    case 79:
    case 78:
    case 77:
    case 74:
    case 73:
    case 72:
    case 69:
    case 68:
    case 67:
    case 64:
    case 63:
    case 62:
    case 61:
    case 60:
    case 58:
    case 57:
    case 56:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 43:
    case 42:
    case 41:
    case 40:
    case 39:
    case 38:
    case 37:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 15 /* 0xf */;

    case 2:
      return 1;

    case 3:
      return 1;

    default:
      abort ();
    }
}

static int athlon_ieu_unit_conflict_cost PARAMS ((rtx, rtx));
static int
athlon_ieu_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 643:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (which_alternative == 3))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      casenum = 1;
      break;

    case 420:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((which_alternative == 2) || (get_attr_type (insn) == TYPE_ISHIFT)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if ((((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) || ((which_alternative != 0) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if ((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_ISHIFT))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ISHIFT)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 2;
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) || ((which_alternative == 3) || (incdec_operand (operands[2], QImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) || ((which_alternative == 2) || (incdec_operand (operands[2], HImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || (((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode))) || (get_attr_type (insn) == TYPE_INCDEC)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 89:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode)))) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode)))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 76:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 71:
      extract_constrain_insn_cached (insn);
      if (((q_regs_operand (operands[0], QImode)) && (! ((TARGET_MOVX) != (0)))) || ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 70:
    case 66:
    case 65:
      if ((get_attr_type (insn) == TYPE_IMOV) || (get_attr_type (insn) == TYPE_IMOVX))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 50:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMOV) || ((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))) || (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
    case 659:
    case 658:
    case 640:
    case 639:
    case 638:
    case 637:
    case 636:
    case 605:
    case 547:
    case 546:
    case 545:
    case 544:
    case 529:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 423:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 294:
    case 293:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 242:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 115:
    case 113:
    case 112:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 86:
    case 85:
    case 84:
    case 81:
    case 80:
    case 79:
    case 78:
    case 77:
    case 74:
    case 73:
    case 72:
    case 69:
    case 68:
    case 67:
    case 64:
    case 63:
    case 62:
    case 61:
    case 60:
    case 58:
    case 57:
    case 56:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 43:
    case 42:
    case 41:
    case 40:
    case 39:
    case 38:
    case 37:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 15 /* 0xf */;

    case 2:
      return 1;

    case 3:
      return 1;

    default:
      abort ();
    }
}

static int athlon_vectordec_unit_blockage PARAMS ((rtx, rtx));
static int
athlon_vectordec_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 926:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 171:
    case 168:
    case 165:
    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 134:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 170:
    case 167:
    case 164:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 132:
    case 131:
    case 130:
    case 129:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 127:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)) && (which_alternative == 1)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 103:
    case 102:
    case 101:
    case 100:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) || ((which_alternative == 4) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && ((get_attr_memory (insn) == MEMORY_LOAD) || (get_attr_memory (insn) == MEMORY_STORE)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 94:
    case 93:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 1) || (memory_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 4)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 714:
    case 643:
    case 641:
    case 82:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (memory_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 77:
    case 58:
    case 57:
    case 49:
    case 48:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (memory_operand (operands[1], VOIDmode))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 924:
    case 854:
    case 791:
    case 789:
    case 787:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 645:
    case 644:
    case 642:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 412:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
    case 225:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 86:
    case 79:
    case 78:
    case 75:
    case 47:
    case 41:
    case 40:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      casenum = 0;
      break;

    default:
      casenum = 1;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      switch (recog_memoized (insn))
	{
        case 926:
	  extract_constrain_insn_cached (insn);
	  if (which_alternative == 1)
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 178:
        case 177:
        case 176:
        case 175:
        case 174:
        case 173:
	  extract_constrain_insn_cached (insn);
	  if ((which_alternative == 0) && ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE))))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 171:
        case 168:
        case 165:
        case 162:
	  extract_constrain_insn_cached (insn);
	  if (which_alternative != 1)
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 134:
	  extract_constrain_insn_cached (insn);
	  if ((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 3)))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 170:
        case 167:
        case 164:
        case 161:
        case 144:
        case 142:
        case 140:
        case 138:
        case 133:
	  extract_constrain_insn_cached (insn);
	  if (which_alternative == 0)
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 132:
        case 131:
        case 130:
        case 129:
	  extract_constrain_insn_cached (insn);
	  if ((which_alternative != 1) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE))))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 127:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 0) && (which_alternative != 1)) || (((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE))) || (which_alternative != 1)))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 103:
        case 102:
        case 101:
        case 100:
	  extract_constrain_insn_cached (insn);
	  if ((which_alternative != 3) && ((which_alternative != 4) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) || ((! (get_attr_memory (insn) == MEMORY_LOAD)) && (! (get_attr_memory (insn) == MEMORY_STORE))))))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 94:
        case 93:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4)))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 88:
        case 87:
	  extract_constrain_insn_cached (insn);
	  if ((which_alternative == 1) && (! (memory_operand (operands[1], VOIDmode))))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 83:
	  extract_constrain_insn_cached (insn);
	  if (which_alternative != 4)
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 714:
        case 643:
        case 641:
        case 82:
	  extract_constrain_insn_cached (insn);
	  if ((which_alternative != 0) && (which_alternative != 1))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 76:
	  extract_constrain_insn_cached (insn);
	  if ((which_alternative == 0) && (! (memory_operand (operands[1], VOIDmode))))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case 77:
        case 58:
        case 57:
        case 49:
        case 48:
        case 39:
        case 38:
        case 37:
	  extract_insn_cached (insn);
	  if (! (memory_operand (operands[1], VOIDmode)))
	    {
	      return 0;
	    }
	  else
	    {
	      return 1;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        case 1024:
        case 1023:
        case 924:
        case 854:
        case 791:
        case 789:
        case 787:
        case 708:
        case 707:
        case 706:
        case 705:
        case 704:
        case 703:
        case 702:
        case 701:
        case 700:
        case 699:
        case 698:
        case 697:
        case 696:
        case 695:
        case 694:
        case 693:
        case 692:
        case 681:
        case 680:
        case 673:
        case 672:
        case 671:
        case 670:
        case 669:
        case 668:
        case 667:
        case 666:
        case 665:
        case 664:
        case 663:
        case 662:
        case 661:
        case 660:
        case 656:
        case 655:
        case 653:
        case 652:
        case 650:
        case 649:
        case 647:
        case 646:
        case 645:
        case 644:
        case 642:
        case 635:
        case 634:
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
        case 622:
        case 621:
        case 620:
        case 619:
        case 618:
        case 617:
        case 616:
        case 615:
        case 614:
        case 613:
        case 612:
        case 611:
        case 610:
        case 609:
        case 608:
        case 607:
        case 606:
        case 605:
        case 604:
        case 603:
        case 602:
        case 601:
        case 600:
        case 599:
        case 598:
        case 597:
        case 596:
        case 595:
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
        case 530:
        case 528:
        case 527:
        case 526:
        case 525:
        case 524:
        case 523:
        case 522:
        case 521:
        case 514:
        case 513:
        case 512:
        case 511:
        case 510:
        case 509:
        case 508:
        case 507:
        case 506:
        case 505:
        case 456:
        case 455:
        case 429:
        case 428:
        case 412:
        case 411:
        case 410:
        case 388:
        case 387:
        case 386:
        case 385:
        case 384:
        case 383:
        case 382:
        case 381:
        case 380:
        case 379:
        case 369:
        case 368:
        case 367:
        case 366:
        case 365:
        case 364:
        case 363:
        case 362:
        case 361:
        case 360:
        case 349:
        case 285:
        case 284:
        case 275:
        case 274:
        case 273:
        case 272:
        case 271:
        case 270:
        case 269:
        case 268:
        case 267:
        case 266:
        case 265:
        case 264:
        case 263:
        case 262:
        case 261:
        case 260:
        case 259:
        case 258:
        case 257:
        case 256:
        case 255:
        case 254:
        case 253:
        case 252:
        case 251:
        case 250:
        case 249:
        case 248:
        case 247:
        case 246:
        case 245:
        case 225:
        case 179:
        case 160:
        case 159:
        case 118:
        case 114:
        case 99:
        case 98:
        case 97:
        case 96:
        case 92:
        case 91:
        case 86:
        case 79:
        case 78:
        case 75:
        case 47:
        case 41:
        case 40:
        case 36:
        case 35:
        case 34:
        case 33:
        case 32:
        case 31:
        case 30:
        case 29:
        case 28:
        case 26:
        case 25:
        case 22:
        case 18:
	  return 1;

        default:
	  return 0;

      }

    default:
      abort ();
    }
}

static int k6_fpu_unit_blockage PARAMS ((rtx, rtx));
static int
k6_fpu_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], TFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], XFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 562:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], DFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 557:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], SFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 555:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], TFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 554:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], XFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 0) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], SFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 2;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 2;

    case 1:
      return 2;

    case 2:
      return 56 /* 0x38 */;

    default:
      abort ();
    }
}

static int k6_fpu_unit_conflict_cost PARAMS ((rtx, rtx));
static int
k6_fpu_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], TFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], XFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 562:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], DFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 557:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], SFmode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 555:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], TFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 554:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], XFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 0) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], SFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 2;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 2;

    case 1:
      return 2;

    case 2:
      return 56 /* 0x38 */;

    default:
      abort ();
    }
}

static int k6_store_unit_blockage PARAMS ((rtx, rtx));
static int
k6_store_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], DImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], SImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      casenum = 1;
      break;

    case 417:
    case 414:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 3)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 420:
    case 209:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      casenum = 0;
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 2;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 10 /* 0xa */;

    case 2:
      return 1;

    default:
      abort ();
    }
}

static int k6_store_unit_conflict_cost PARAMS ((rtx, rtx));
static int
k6_store_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], DImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], SImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
      casenum = 1;
      break;

    case 417:
    case 414:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 3)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 420:
    case 209:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      casenum = 0;
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 2;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 2;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 10 /* 0xa */;

    case 2:
      return 1;

    default:
      abort ();
    }
}

static int k6_load_unit_blockage PARAMS ((rtx, rtx));
static int
k6_load_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (! (constant_call_address_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (! (constant_call_address_operand (operands[0], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if (memory_operand (operands[0], VOIDmode))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (memory_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (memory_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 851:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 547:
    case 546:
    case 545:
    case 544:
    case 500:
    case 499:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 85:
    case 79:
    case 78:
    case 68:
    case 52:
    case 46:
    case 41:
    case 40:
      casenum = 0;
      break;

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 77:
    case 62:
    case 58:
    case 57:
    case 56:
    case 49:
    case 48:
    case 43:
    case 42:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (memory_operand (operands[1], VOIDmode))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if (get_attr_memory (insn) == MEMORY_LOAD)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 854:
    case 853:
    case 852:
    case 850:
    case 814:
    case 813:
    case 770:
    case 769:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 645:
    case 644:
    case 642:
    case 638:
    case 636:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 84:
    case 75:
    case 67:
    case 51:
    case 45:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      casenum = 1;
      break;

    default:
      casenum = 0;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 10 /* 0xa */;

    default:
      abort ();
    }
}

static int k6_load_unit_conflict_cost PARAMS ((rtx, rtx));
static int
k6_load_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 679:
    case 678:
    case 677:
    case 676:
    case 675:
    case 674:
      extract_insn_cached (insn);
      if (! (constant_call_address_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 525:
    case 524:
    case 523:
    case 522:
    case 521:
      extract_insn_cached (insn);
      if (! (constant_call_address_operand (operands[0], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
      extract_insn_cached (insn);
      if (memory_operand (operands[0], VOIDmode))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 88:
    case 87:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (memory_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 76:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (memory_operand (operands[1], VOIDmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 851:
    case 617:
    case 616:
    case 615:
    case 614:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 547:
    case 546:
    case 545:
    case 544:
    case 500:
    case 499:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 85:
    case 79:
    case 78:
    case 68:
    case 52:
    case 46:
    case 41:
    case 40:
      casenum = 0;
      break;

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 77:
    case 62:
    case 58:
    case 57:
    case 56:
    case 49:
    case 48:
    case 43:
    case 42:
    case 39:
    case 38:
    case 37:
      extract_insn_cached (insn);
      if (memory_operand (operands[1], VOIDmode))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 967:
    case 966:
    case 965:
    case 964:
    case 963:
    case 962:
    case 904:
    case 903:
    case 902:
    case 901:
    case 900:
    case 899:
    case 860:
    case 859:
    case 858:
    case 828:
    case 827:
    case 826:
    case 825:
    case 824:
    case 823:
    case 776:
    case 775:
    case 774:
    case 773:
    case 772:
    case 771:
    case 502:
    case 501:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 36:
    case 35:
    case 34:
    case 33:
    case 32:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      if (get_attr_memory (insn) == MEMORY_LOAD)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1024:
    case 1023:
    case 1022:
    case 1021:
    case 1020:
    case 953:
    case 884:
    case 883:
    case 882:
    case 881:
    case 864:
    case 854:
    case 853:
    case 852:
    case 850:
    case 814:
    case 813:
    case 770:
    case 769:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 703:
    case 702:
    case 701:
    case 700:
    case 699:
    case 698:
    case 697:
    case 696:
    case 695:
    case 694:
    case 693:
    case 692:
    case 681:
    case 680:
    case 673:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 666:
    case 665:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 645:
    case 644:
    case 642:
    case 638:
    case 636:
    case 635:
    case 634:
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
    case 622:
    case 621:
    case 620:
    case 619:
    case 618:
    case 605:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
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
    case 530:
    case 528:
    case 527:
    case 526:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 456:
    case 455:
    case 429:
    case 428:
    case 411:
    case 410:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 380:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 364:
    case 363:
    case 362:
    case 361:
    case 360:
    case 349:
    case 285:
    case 284:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 225:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 179:
    case 160:
    case 159:
    case 118:
    case 114:
    case 99:
    case 98:
    case 97:
    case 96:
    case 92:
    case 91:
    case 84:
    case 75:
    case 67:
    case 51:
    case 45:
    case 30:
    case 29:
    case 28:
    case 26:
    case 25:
    case 22:
    case 18:
      casenum = 1;
      break;

    default:
      casenum = 0;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 10 /* 0xa */;

    default:
      abort ();
    }
}

static int k6_alu_unit_blockage PARAMS ((rtx, rtx));
static int
k6_alu_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 1) || (! (const0_operand (operands[2], DImode))))
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 1) || (! (const0_operand (operands[2], SImode))))
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 420:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ISHIFT) || ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if (((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))) || ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if ((get_attr_type (insn) == TYPE_ISHIFT) || (get_attr_type (insn) == TYPE_ALU))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ISHIFT) || ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 2;
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) || ((incdec_operand (operands[2], QImode)) || (which_alternative == 3)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) || ((incdec_operand (operands[2], HImode)) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 115:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 71:
      extract_constrain_insn_cached (insn);
      if ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 70:
    case 66:
    case 65:
      if (get_attr_type (insn) == TYPE_IMOVX)
        {
	  casenum = 0;
        }
      else if ((get_attr_type (insn) == TYPE_IMOV) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 59:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2)))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 50:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2))))
        {
	  casenum = 0;
        }
      else if ((get_attr_type (insn) == TYPE_IMOV) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 86:
    case 74:
    case 73:
    case 72:
    case 61:
    case 60:
    case 55:
    case 54:
    case 53:
    case 47:
      if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 638:
    case 636:
    case 547:
    case 546:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 423:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 294:
    case 293:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 242:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 112:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 81:
    case 80:
    case 69:
    case 64:
    case 63:
    case 62:
    case 56:
    case 43:
    case 42:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 1;

    case 2:
      return 2;

    case 3:
      return 17 /* 0x11 */;

    default:
      abort ();
    }
}

static int k6_alu_unit_conflict_cost PARAMS ((rtx, rtx));
static int
k6_alu_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 1) || (! (const0_operand (operands[2], DImode))))
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 1) || (! (const0_operand (operands[2], SImode))))
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 420:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ISHIFT) || ((get_attr_type (insn) == TYPE_ALU) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if (((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))) || ((which_alternative != 0) || (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if ((get_attr_type (insn) == TYPE_ISHIFT) || (get_attr_type (insn) == TYPE_ALU))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ISHIFT) || ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ALU)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 2;
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && (! (incdec_operand (operands[2], QImode)))) || ((incdec_operand (operands[2], QImode)) || (which_alternative == 3)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && (! (incdec_operand (operands[2], HImode)))) || ((incdec_operand (operands[2], HImode)) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ALU) || ((get_attr_type (insn) == TYPE_INCDEC) || ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 115:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 89:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], DImode))))))))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 71:
      extract_constrain_insn_cached (insn);
      if ((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0)))
        {
	  casenum = 0;
        }
      else if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 70:
    case 66:
    case 65:
      if (get_attr_type (insn) == TYPE_IMOVX)
        {
	  casenum = 0;
        }
      else if ((get_attr_type (insn) == TYPE_IMOV) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 59:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2)))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative == 3) && (((TARGET_PARTIAL_REG_STALL) == (0)) || ((TARGET_QIMODE_MATH) == (0)))) || (((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative != 3) && ((which_alternative != 5) && ((! ((TARGET_MOVX) != (0))) || (which_alternative != 2)))))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 50:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2))))
        {
	  casenum = 0;
        }
      else if ((get_attr_type (insn) == TYPE_IMOV) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 86:
    case 74:
    case 73:
    case 72:
    case 61:
    case 60:
    case 55:
    case 54:
    case 53:
    case 47:
      if (get_attr_memory (insn) == MEMORY_NONE)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((! ((flag_pic) != (0))) || (! (symbolic_operand (operands[1], SImode)))))))) && (get_attr_memory (insn) == MEMORY_NONE))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 638:
    case 636:
    case 547:
    case 546:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 423:
    case 412:
    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 348:
    case 347:
    case 346:
    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 294:
    case 293:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 242:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 112:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 81:
    case 80:
    case 69:
    case 64:
    case 63:
    case 62:
    case 56:
    case 43:
    case 42:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 1;

    case 2:
      return 2;

    case 3:
      return 17 /* 0x11 */;

    default:
      abort ();
    }
}

static int k6_alux_unit_blockage PARAMS ((rtx, rtx));
static int
k6_alux_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 0) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 605:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
      casenum = 0;
      break;

    case 422:
    case 421:
    case 420:
    case 419:
    case 418:
    case 417:
    case 415:
    case 413:
    case 409:
    case 408:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_ISHIFT)
        {
	  casenum = 0;
        }
      else if (get_attr_type (insn) == TYPE_ALU)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 2;
      break;

    case 217:
      extract_insn_cached (insn);
      if (! (incdec_operand (operands[2], QImode)))
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 3) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 2) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 202:
    case 201:
    case 196:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_INCDEC)) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 659:
    case 658:
    case 115:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 112:
    case 109:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 71:
      extract_constrain_insn_cached (insn);
      if (((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 70:
    case 66:
    case 65:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMOVX) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 50:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      extract_insn_cached (insn);
      casenum = 0;
      break;

    case 638:
    case 636:
    case 547:
    case 546:
    case 348:
    case 347:
    case 346:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 318:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 296:
    case 294:
    case 293:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 110:
    case 107:
    case 69:
    case 64:
    case 63:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      extract_insn_cached (insn);
      if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 1;

    case 2:
      return 2;

    case 3:
      return 17 /* 0x11 */;

    default:
      abort ();
    }
}

static int k6_alux_unit_conflict_cost PARAMS ((rtx, rtx));
static int
k6_alux_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 0) && (((TARGET_DOUBLE_WITH_ADD) != (0)) && (const1_operand (operands[2], VOIDmode)))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 605:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
      casenum = 0;
      break;

    case 422:
    case 421:
    case 420:
    case 419:
    case 418:
    case 417:
    case 415:
    case 413:
    case 409:
    case 408:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_ISHIFT)
        {
	  casenum = 0;
        }
      else if (get_attr_type (insn) == TYPE_ALU)
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 2;
      break;

    case 217:
      extract_insn_cached (insn);
      if (! (incdec_operand (operands[2], QImode)))
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 3) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 2) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 202:
    case 201:
    case 196:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_ALU) || (get_attr_type (insn) == TYPE_INCDEC)) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 659:
    case 658:
    case 115:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 112:
    case 109:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 71:
      extract_constrain_insn_cached (insn);
      if (((! (q_regs_operand (operands[0], QImode))) || ((TARGET_MOVX) != (0))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 70:
    case 66:
    case 65:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMOVX) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 59:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 3) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_QIMODE_MATH) == (0))))) && ((which_alternative == 3) || ((which_alternative == 5) || (((TARGET_MOVX) != (0)) && (which_alternative == 2))))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 50:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 0) || ((! ((TARGET_PARTIAL_REG_STALL) == (0))) && (! ((TARGET_HIMODE_MATH) == (0))))) && (((which_alternative != 1) && (which_alternative != 2)) || (! (aligned_operand (operands[1], HImode))))) && (((TARGET_MOVX) != (0)) && ((which_alternative == 0) || (which_alternative == 2)))) && (general_operand (operands[0], QImode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 407:
    case 406:
    case 405:
    case 404:
    case 403:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 359:
    case 358:
    case 357:
    case 356:
    case 355:
    case 354:
    case 353:
    case 352:
    case 351:
    case 350:
    case 345:
    case 339:
    case 319:
    case 317:
    case 297:
    case 295:
    case 242:
    case 111:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      extract_insn_cached (insn);
      casenum = 0;
      break;

    case 638:
    case 636:
    case 547:
    case 546:
    case 348:
    case 347:
    case 346:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 318:
    case 316:
    case 315:
    case 314:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 296:
    case 294:
    case 293:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 244:
    case 243:
    case 241:
    case 240:
    case 239:
    case 238:
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
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 216:
    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 208:
    case 207:
    case 206:
    case 205:
    case 204:
    case 203:
    case 200:
    case 199:
    case 198:
    case 197:
    case 185:
    case 184:
    case 183:
    case 182:
    case 181:
    case 180:
    case 126:
    case 125:
    case 124:
    case 123:
    case 122:
    case 121:
    case 120:
    case 119:
    case 117:
    case 116:
    case 113:
    case 110:
    case 107:
    case 69:
    case 64:
    case 63:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    case 0:
      extract_insn_cached (insn);
      if (general_operand (operands[0], QImode))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 1;

    case 2:
      return 2;

    case 3:
      return 17 /* 0x11 */;

    default:
      abort ();
    }
}

static int fpu_unit_blockage PARAMS ((rtx, rtx));
static int
fpu_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
      casenum = 2;
      break;

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], TFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], XFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 562:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], DFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 557:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], SFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 555:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], TFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 554:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], XFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 0) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], SFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 643:
    case 642:
    case 641:
    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 645:
    case 644:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 2;

    case 2:
      return 56 /* 0x38 */;

    case 3:
      return 1;

    default:
      abort ();
    }
}

static int fpu_unit_conflict_cost PARAMS ((rtx, rtx));
static int
fpu_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 598:
    case 597:
    case 596:
    case 595:
    case 594:
    case 593:
    case 592:
    case 591:
    case 590:
    case 589:
    case 588:
    case 587:
    case 584:
      casenum = 2;
      break;

    case 585:
    case 582:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], TFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], XFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 562:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], DFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 557:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 0;
        }
      else if (mult_operator (operands[3], SFmode))
        {
	  casenum = 1;
        }
      else if (get_attr_type (insn) == TYPE_FDIV)
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 555:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], TFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 554:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], XFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 0) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], SFmode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 1;
        }
      break;

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 643:
    case 642:
    case 641:
    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 3;
        }
      break;

    case 645:
    case 644:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 3;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 2;

    case 2:
      return 56 /* 0x38 */;

    case 3:
      return 1;

    default:
      abort ();
    }
}

static int ppro_p0_unit_blockage PARAMS ((rtx, rtx));
static int
ppro_p0_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], DImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], SImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 645:
    case 644:
      casenum = 4;
      break;

    case 643:
    case 642:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], TFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], XFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 562:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], DFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 557:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], SFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 555:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], TFmode)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 7;
        }
      break;

    case 554:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], XFmode)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 7;
        }
      break;

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 0) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], SFmode)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 7;
        }
      break;

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 420:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ISHIFT) || (which_alternative == 2))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if (get_attr_type (insn) == TYPE_ISHIFT)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ISHIFT))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
      casenum = 2;
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 1;
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 3)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 605:
    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      casenum = 0;
      break;

    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      casenum = 3;
      break;

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
      casenum = 6;
      break;

    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      casenum = 5;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 8;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 1;

    case 2:
      return 17 /* 0x11 */;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 1;

    case 6:
      return 1;

    case 7:
      return 1;

    case 8:
      return 1;

    default:
      abort ();
    }
}

static int ppro_p0_unit_conflict_cost PARAMS ((rtx, rtx));
static int
ppro_p0_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 659:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], DImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 658:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (! (const0_operand (operands[2], SImode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 645:
    case 644:
      casenum = 4;
      break;

    case 643:
    case 642:
    case 641:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 581:
    case 579:
    case 577:
    case 575:
    case 573:
    case 571:
    case 569:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], TFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 580:
    case 578:
    case 576:
    case 574:
    case 572:
    case 570:
    case 568:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], XFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 562:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], DFmode)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 567:
    case 566:
    case 565:
    case 564:
    case 561:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], DFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 557:
      extract_constrain_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if ((which_alternative != 2) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 560:
    case 559:
    case 556:
      extract_insn_cached (insn);
      if (get_attr_type (insn) == TYPE_FOP)
        {
	  casenum = 3;
        }
      else if (mult_operator (operands[3], SFmode))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 555:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], TFmode)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 7;
        }
      break;

    case 554:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], XFmode)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 7;
        }
      break;

    case 552:
    case 549:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (! (mult_operator (operands[3], SFmode))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 0) && (mult_operator (operands[3], SFmode)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 551:
    case 548:
      extract_insn_cached (insn);
      if (! (mult_operator (operands[3], SFmode)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 7;
        }
      break;

    case 432:
    case 431:
    case 423:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 420:
      extract_constrain_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_ISHIFT) || (which_alternative == 2))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 416:
      extract_constrain_insn_cached (insn);
      if ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 414:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || ((! ((TARGET_DOUBLE_WITH_ADD) != (0))) || (! (const1_operand (operands[2], VOIDmode)))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 422:
    case 421:
    case 419:
    case 418:
    case 415:
    case 409:
      if (get_attr_type (insn) == TYPE_ISHIFT)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 417:
    case 413:
    case 408:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (get_attr_type (insn) == TYPE_ISHIFT))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 263:
    case 262:
      casenum = 2;
      break;

    case 261:
    case 260:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
      casenum = 1;
      break;

    case 215:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 3)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 209:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 2)
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 202:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 201:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], SImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 196:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) || (pic_symbolic_operand (operands[2], DImode)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 605:
    case 529:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 504:
    case 503:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 474:
    case 473:
    case 472:
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
    case 457:
    case 454:
    case 453:
    case 452:
    case 451:
    case 450:
    case 449:
    case 448:
    case 447:
    case 446:
    case 445:
    case 444:
    case 443:
    case 442:
    case 441:
    case 440:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 430:
    case 427:
    case 426:
    case 425:
    case 424:
    case 412:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 187:
    case 186:
      casenum = 0;
      break;

    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 378:
    case 377:
    case 376:
    case 375:
    case 374:
    case 373:
    case 372:
    case 371:
    case 370:
    case 158:
    case 157:
    case 156:
    case 153:
    case 152:
    case 151:
    case 148:
    case 147:
    case 146:
      casenum = 3;
      break;

    case 135:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 1)
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 178:
    case 177:
    case 176:
    case 175:
    case 174:
    case 173:
    case 171:
    case 170:
    case 168:
    case 167:
    case 165:
    case 164:
    case 162:
    case 161:
    case 144:
    case 142:
    case 140:
    case 138:
    case 134:
    case 133:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 145:
    case 143:
    case 141:
    case 139:
    case 136:
    case 132:
    case 131:
    case 130:
    case 129:
      casenum = 6;
      break;

    case 127:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 103:
    case 102:
    case 101:
    case 100:
    case 94:
    case 93:
    case 89:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  casenum = 6;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 83:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (which_alternative != 6)) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 4) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], DImode))))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 44:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (((flag_pic) != (0)) && (symbolic_operand (operands[1], SImode)))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 35:
    case 32:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 34:
    case 31:
    case 27:
    case 24:
    case 23:
    case 21:
    case 20:
    case 19:
      casenum = 5;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 8;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 1;

    case 2:
      return 17 /* 0x11 */;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 1;

    case 6:
      return 1;

    case 7:
      return 1;

    case 8:
      return 1;

    default:
      abort ();
    }
}

const struct function_unit_desc function_units[] = {
  {"ppro_p0", 1, 1, 0, 0, 17, ppro_p0_unit_ready_cost, ppro_p0_unit_conflict_cost, 17, ppro_p0_unit_blockage_range, ppro_p0_unit_blockage}, 
  {"ppro_p01", 2, 2, 0, 1, 1, ppro_p01_unit_ready_cost, 0, 1, 0, 0}, 
  {"ppro_p2", 4, 1, 0, 1, 1, ppro_p2_unit_ready_cost, 0, 1, 0, 0}, 
  {"ppro_p34", 8, 1, 0, 1, 1, ppro_p34_unit_ready_cost, 0, 1, 0, 0}, 
  {"fpu", 16, 1, 0, 0, 56, fpu_unit_ready_cost, fpu_unit_conflict_cost, 56, fpu_unit_blockage_range, fpu_unit_blockage}, 
  {"k6_alux", 32, 1, 0, 0, 17, k6_alux_unit_ready_cost, k6_alux_unit_conflict_cost, 17, k6_alux_unit_blockage_range, k6_alux_unit_blockage}, 
  {"k6_alu", 64, 2, 0, 0, 17, k6_alu_unit_ready_cost, k6_alu_unit_conflict_cost, 17, k6_alu_unit_blockage_range, k6_alu_unit_blockage}, 
  {"k6_branch", 128, 1, 0, 1, 1, k6_branch_unit_ready_cost, 0, 1, 0, 0}, 
  {"k6_load", 256, 1, 0, 0, 10, k6_load_unit_ready_cost, k6_load_unit_conflict_cost, 10, k6_load_unit_blockage_range, k6_load_unit_blockage}, 
  {"k6_store", 512, 1, 0, 0, 10, k6_store_unit_ready_cost, k6_store_unit_conflict_cost, 10, k6_store_unit_blockage_range, k6_store_unit_blockage}, 
  {"k6_fpu", 1024, 1, 1, 0, 56, k6_fpu_unit_ready_cost, k6_fpu_unit_conflict_cost, 56, k6_fpu_unit_blockage_range, k6_fpu_unit_blockage}, 
  {"athlon_vectordec", 2048, 1, 0, 1, 1, athlon_vectordec_unit_ready_cost, 0, 1, athlon_vectordec_unit_blockage_range, athlon_vectordec_unit_blockage}, 
  {"athlon_directdec", 4096, 3, 0, 1, 1, athlon_directdec_unit_ready_cost, 0, 1, 0, 0}, 
  {"athlon_ieu", 8192, 3, 0, 0, 15, athlon_ieu_unit_ready_cost, athlon_ieu_unit_conflict_cost, 15, athlon_ieu_unit_blockage_range, athlon_ieu_unit_blockage}, 
  {"athlon_muldiv", 16384, 1, 0, 0, 42, athlon_muldiv_unit_ready_cost, athlon_muldiv_unit_conflict_cost, 42, athlon_muldiv_unit_blockage_range, athlon_muldiv_unit_blockage}, 
  {"athlon_fp", 32768, 3, 0, 1, 1, athlon_fp_unit_ready_cost, 0, 1, 0, 0}, 
  {"athlon_fp_mul", 65536, 1, 0, 1, 1, athlon_fp_mul_unit_ready_cost, 0, 1, 0, 0}, 
  {"athlon_fp_add", 131072, 1, 0, 1, 1, athlon_fp_add_unit_ready_cost, 0, 1, 0, 0}, 
  {"athlon_fp_muladd", 262144, 2, 0, 1, 1, athlon_fp_muladd_unit_ready_cost, 0, 1, 0, 0}, 
  {"athlon_fp_store", 524288, 1, 0, 1, 1, athlon_fp_store_unit_ready_cost, 0, 1, 0, 0}, 
  {"athlon_load", 1048576, 2, 0, 1, 1, athlon_load_unit_ready_cost, 0, 1, 0, 0}, 
};


int max_dfa_issue_rate = 2;
/* Vector translating external insn codes to internal ones.*/
static const unsigned char pentium_translate[] ATTRIBUTE_UNUSED = {
    0,     1,     2,     3,     2,     4,     3,     5,     5,     5,
    6,     7,     8,     5,     2,     2,     9,    10,    11,     4,
   12,    13,    14,     3,     5,     8,     7,     2,    15};

/* Comb vector for state transitions.  */
static const unsigned char pentium_transitions[] ATTRIBUTE_UNUSED = {
   19,    18,     2,     3,     4,    15,    17,    16,    15,    14,
   14,    13,     5,     5,     1,     0,     3,     6,     3,     0,
    4,     2,     4,     3,     3,     3,     2,     2,     6,     2,
    4,     4,     7,     4,     3,     3,     3,     0,     8,     4,
    3,     9,     3,    10,    11,     2,     4,     6,     4,    12,
    2,     6,     4,    19,     4,     6,     3,     3,     6,     6,
    0,     7};

/* Check vector for state transitions.  */
static const unsigned char pentium_check[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     5,     5,     5,     2,
    5,     3,     5,     5,     4,     5,     5,    15,    15,    15,
   13,    15,     6,    15,    15,     1,    15,    15,     7,     1,
   13,     8,     1,     9,    10,     1,    14,    14,    14,    11,
   16,    17,    12,    18,    16,    17,    14,    16,    17,    19,
   16,    17};

/* Base vector for state transitions.  */
static const unsigned char pentium_base[] = {
    0,    30,     4,     6,     9,    11,    17,    23,    26,    28,
   29,    34,    37,    25,    41,    22,    45,    46,    38,    44,
};


#if AUTOMATON_STATE_ALTS
/* Comb vector for state insn alternatives.  */
static const unsigned char pentium_state_alts[] ATTRIBUTE_UNUSED = {
    1,     1,     1,     1,     1,     2,     1,     1,     1,    24,
    2,     2,    12,     2,     2,     1,     1,     1,     1,     1,
   10,     1,     1,     5,     1,     1,     1,     1,     1,     1,
    1,    16,     1,     2,     8,     1,     2,     1,     1,     2,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     8,     2,     1,     4,     2,     1,
    1,     1};

/* Check vector for state insn alternatives.  */
static const unsigned char pentium_check_state_alts[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     5,     5,     5,     2,
    5,     3,     5,     5,     4,     5,     5,    15,    15,    15,
   13,    15,     6,    15,    15,     1,    15,    15,     7,     1,
   13,     8,     1,     9,    10,     1,    14,    14,    14,    11,
   16,    17,    12,    18,    16,    17,    14,    16,    17,    19,
   16,    17};

/* Base vector for state insn alternatives.  */
static const unsigned char pentium_base_state_alts[] = {
    0,    30,     4,     6,     9,    11,    17,    23,    26,    28,
   29,    34,    37,    25,    41,    22,    45,    46,    38,    44,
};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char pentium_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,    34,    34,
   32,    34,    32,    34,     2,    32,    17,    17,    17,    17,
   17,    17,    17,    16,    34,    34,    34,    34,    34,    34,
   34,    32,    51,    51,    51,    51,    51,    51,    51,    48,
   34,    34,    32,     0,    32,    32,     2,     0,   170,   170,
  170,   170,   170,   170,   170,   160,   153,   153,   153,   153,
  153,   153,   153,   144,   136,   136,   136,   136,   136,   136,
  136,   128,   119,   119,   119,   119,   119,   119,   119,   112,
  102,   102,   102,   102,   102,   102,   102,    96,    85,    85,
   85,    85,    85,    85,    85,    80,    68,    68,    68,    68,
   68,    68,    68,    64,    51,    51,    48,    51,    51,    51,
   51,    48,    51,    51,    48,     0,    51,    51,    51,    48,
   17,    17,    16,     0,    16,    16,     1,     0,    17,    17,
   16,    17,    16,    17,     1,    16,   170,   170,   160,   170,
  160,   170,    10,   160,   204,   204,   204,   204,   204,   204,
  204,   192,   187,   187,   187,   187,   187,   187,   187,   176,
};

/* Vector for locked state flags.  */
static const unsigned char pentium_dead_lock[] = {
    0,     0,     1,     1,     1,     0,     1,     1,     1,     1,
    1,     1,     1,     0,     0,     0,     0,     0,     1,     1,
};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char pentium_fpu_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     1,     0,     1,     2,     3,     0,     0,     0,
    0,     0,     1,     4,     5,     6,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     7};

/* Comb vector for state transitions.  */
static const unsigned char pentium_fpu_transitions[] ATTRIBUTE_UNUSED = {
    0,    72,    73,    71,    74,    32,     1,     0,    69,    74,
   73,    71,    75,     1,     2,    70,    70,    72,    73,    71,
    2,     3,     3,     0,     4,     5,     6,     7,     8,     4,
    9,     5,     6,     7,     8,     9,    10,    10,    11,    12,
   13,    14,    15,    11,    16,    12,    13,    14,    15,    16,
   17,    17,    18,    19,    20,    21,    22,    18,    23,    19,
   20,    21,    22,    23,    24,    24,    25,    26,    27,    28,
   29,    25,    30,    26,    27,    28,    29,    30,    31,    31,
   32,    33,    34,    35,    36,    32,    37,    33,    34,    35,
   36,    37,    38,    38,    39,    40,    41,    42,    43,    39,
   44,    40,    41,    42,    43,    44,    45,    45,    46,    47,
   48,    49,    50,    46,    51,    47,    48,    49,    50,    51,
   52,    52,    53,    54,    55,    56,    57,    53,    58,    54,
   55,    56,    57,    58,    59,    59,    60,    61,    62,    63,
   64,    60,    65,    61,    62,    63,    64,    65,    66,    66,
   67,    68,    71,    72,    73,    67,    74,    68,    69,    72,
    0,    71,    75,    70};

/* Check vector for state transitions.  */
static const unsigned char pentium_fpu_check[] = {
    0,     0,     0,     0,     0,     0,     0,     0,    69,    69,
   69,    69,    75,     1,     2,    69,    70,    70,    70,    70,
    1,     2,     3,    70,     4,     5,     6,     7,     8,     3,
    9,     4,     5,     6,     7,     8,    10,     9,    11,    12,
   13,    14,    15,    10,    16,    11,    12,    13,    14,    15,
   17,    16,    18,    19,    20,    21,    22,    17,    23,    18,
   19,    20,    21,    22,    24,    23,    25,    26,    27,    28,
   29,    24,    30,    25,    26,    27,    28,    29,    31,    30,
   32,    33,    34,    35,    36,    31,    37,    32,    33,    34,
   35,    36,    38,    37,    39,    40,    41,    42,    43,    38,
   44,    39,    40,    41,    42,    43,    45,    44,    46,    47,
   48,    49,    50,    45,    51,    46,    47,    48,    49,    50,
   52,    51,    53,    54,    55,    56,    57,    52,    58,    53,
   54,    55,    56,    57,    59,    58,    60,    61,    62,    63,
   64,    59,    65,    60,    61,    62,    63,    64,    66,    65,
   67,    68,    71,    72,    73,    66,    74,    67,    68,    71,
   72,    73,    75,    74};

/* Base vector for state transitions.  */
static const unsigned char pentium_fpu_base[] = {
    0,    13,    14,    22,    24,    25,    26,    27,    28,    30,
   36,    38,    39,    40,    41,    42,    44,    50,    52,    53,
   54,    55,    56,    58,    64,    66,    67,    68,    69,    70,
   72,    78,    80,    81,    82,    83,    84,    86,    92,    94,
   95,    96,    97,    98,   100,   106,   108,   109,   110,   111,
  112,   114,   120,   122,   123,   124,   125,   126,   128,   134,
  136,   137,   138,   139,   140,   142,   148,   150,   151,     8,
   16,   152,   153,   154,   156};


#if AUTOMATON_STATE_ALTS
/* Comb vector for state insn alternatives.  */
static const unsigned char pentium_fpu_state_alts[] ATTRIBUTE_UNUSED = {
    1,     1,     1,     1,     4,     1,     1,     1,     1,     1,
    1,     1,     0,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     0,     1};

/* Check vector for state insn alternatives.  */
static const unsigned char pentium_fpu_check_state_alts[] = {
    0,     0,     0,     0,     0,     0,     0,     0,    69,    69,
   69,    69,    75,     1,     2,    69,    70,    70,    70,    70,
    1,     2,     3,    70,     4,     5,     6,     7,     8,     3,
    9,     4,     5,     6,     7,     8,    10,     9,    11,    12,
   13,    14,    15,    10,    16,    11,    12,    13,    14,    15,
   17,    16,    18,    19,    20,    21,    22,    17,    23,    18,
   19,    20,    21,    22,    24,    23,    25,    26,    27,    28,
   29,    24,    30,    25,    26,    27,    28,    29,    31,    30,
   32,    33,    34,    35,    36,    31,    37,    32,    33,    34,
   35,    36,    38,    37,    39,    40,    41,    42,    43,    38,
   44,    39,    40,    41,    42,    43,    45,    44,    46,    47,
   48,    49,    50,    45,    51,    46,    47,    48,    49,    50,
   52,    51,    53,    54,    55,    56,    57,    52,    58,    53,
   54,    55,    56,    57,    59,    58,    60,    61,    62,    63,
   64,    59,    65,    60,    61,    62,    63,    64,    66,    65,
   67,    68,    71,    72,    73,    66,    74,    67,    68,    71,
   72,    73,    75,    74};

/* Base vector for state insn alternatives.  */
static const unsigned char pentium_fpu_base_state_alts[] = {
    0,    13,    14,    22,    24,    25,    26,    27,    28,    30,
   36,    38,    39,    40,    41,    42,    44,    50,    52,    53,
   54,    55,    56,    58,    64,    66,    67,    68,    69,    70,
   72,    78,    80,    81,    82,    83,    84,    86,    92,    94,
   95,    96,    97,    98,   100,   106,   108,   109,   110,   111,
  112,   114,   120,   122,   123,   124,   125,   126,   128,   134,
  136,   137,   138,   139,   140,   142,   148,   150,   151,     8,
   16,   152,   153,   154,   156};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char pentium_fpu_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,    68,
   68,    68,    70,    70,    70,     0,     0,    67,    67,    67,
   69,    69,    69,     0,     0,    66,    66,    66,    68,    68,
   68,     0,     0,    65,    65,    65,    67,    67,    67,     0,
    0,    64,    64,    64,    66,    66,    66,     0,     0,    63,
   63,    63,    65,    65,    65,     0,     0,    62,    62,    62,
   64,    64,    64,     0,     0,    61,    61,    61,    63,    63,
   63,     0,     0,    60,    60,    60,    62,    62,    62,     0,
    0,    59,    59,    59,    61,    61,    61,     0,     0,    58,
   58,    58,    60,    60,    60,     0,     0,    57,    57,    57,
   59,    59,    59,     0,     0,    56,    56,    56,    58,    58,
   58,     0,     0,    55,    55,    55,    57,    57,    57,     0,
    0,    54,    54,    54,    56,    56,    56,     0,     0,    53,
   53,    53,    55,    55,    55,     0,     0,    52,    52,    52,
   54,    54,    54,     0,     0,    51,    51,    51,    53,    53,
   53,     0,     0,    50,    50,    50,    52,    52,    52,     0,
    0,    49,    49,    49,    51,    51,    51,     0,     0,    48,
   48,    48,    50,    50,    50,     0,     0,    47,    47,    47,
   49,    49,    49,     0,     0,    46,    46,    46,    48,    48,
   48,     0,     0,    45,    45,    45,    47,    47,    47,     0,
    0,    44,    44,    44,    46,    46,    46,     0,     0,    43,
   43,    43,    45,    45,    45,     0,     0,    42,    42,    42,
   44,    44,    44,     0,     0,    41,    41,    41,    43,    43,
   43,     0,     0,    40,    40,    40,    42,    42,    42,     0,
    0,    39,    39,    39,    41,    41,    41,     0,     0,    38,
   38,    38,    40,    40,    40,     0,     0,    37,    37,    37,
   39,    39,    39,     0,     0,    36,    36,    36,    38,    38,
   38,     0,     0,    35,    35,    35,    37,    37,    37,     0,
    0,    34,    34,    34,    36,    36,    36,     0,     0,    33,
   33,    33,    35,    35,    35,     0,     0,    32,    32,    32,
   34,    34,    34,     0,     0,    31,    31,    31,    33,    33,
   33,     0,     0,    30,    30,    30,    32,    32,    32,     0,
    0,    29,    29,    29,    31,    31,    31,     0,     0,    28,
   28,    28,    30,    30,    30,     0,     0,    27,    27,    27,
   29,    29,    29,     0,     0,    26,    26,    26,    28,    28,
   28,     0,     0,    25,    25,    25,    27,    27,    27,     0,
    0,    24,    24,    24,    26,    26,    26,     0,     0,    23,
   23,    23,    25,    25,    25,     0,     0,    22,    22,    22,
   24,    24,    24,     0,     0,    21,    21,    21,    23,    23,
   23,     0,     0,    20,    20,    20,    22,    22,    22,     0,
    0,    19,    19,    19,    21,    21,    21,     0,     0,    18,
   18,    18,    20,    20,    20,     0,     0,    17,    17,    17,
   19,    19,    19,     0,     0,    16,    16,    16,    18,    18,
   18,     0,     0,    15,    15,    15,    17,    17,    17,     0,
    0,    14,    14,    14,    16,    16,    16,     0,     0,    13,
   13,    13,    15,    15,    15,     0,     0,    12,    12,    12,
   14,    14,    14,     0,     0,    11,    11,    11,    13,    13,
   13,     0,     0,    10,    10,    10,    12,    12,    12,     0,
    0,     9,     9,     9,    11,    11,    11,     0,     0,     8,
    8,     8,    10,    10,    10,     0,     0,     7,     7,     7,
    9,     9,     9,     0,     0,     6,     6,     6,     8,     8,
    8,     0,     0,     5,     5,     5,     7,     7,     7,     0,
    0,     4,     4,     4,     6,     6,     6,     0,     0,     3,
    3,     3,     5,     5,     5,     0,     0,     2,     2,     2,
    4,     4,     4,     0,     0,     1,     1,     1,     3,     3,
    3,     0,     0,     0,     0,     0,     2,     2,     2,     0,
    0,     0,     0,     0,     1,     1,     1,     0,     0,     2,
    2,     2,     2,     2,     2,     0,     0,     1,     1,     1,
    1,     1,     1,     0,     0,     3,     3,     3,     3,     3,
    3,     0,     0,     1,     1,     1,     2,     2,     2,     0,
};

/* Vector for locked state flags.  */
static const unsigned char pentium_fpu_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0};


#define DFA__ADVANCE_CYCLE 28

struct DFA_chip
{
  unsigned char pentium_automaton_state;
  unsigned char pentium_fpu_automaton_state;
};


int max_insn_queue_index = 127;

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
    case 0: /* pent_mul */
    case 1: /* pent_str */
    case 3: /* pent_cld */
    case 7: /* pent_imov */
    case 8: /* pent_push */
    case 9: /* pent_pop */
    case 10: /* pent_call */
    case 11: /* pent_branch */
    case 16: /* pent_uv_both */
    case 17: /* pent_u_both */
    case 18: /* pent_v_both */
    case 19: /* pent_np_both */
    case 20: /* pent_uv_load */
    case 21: /* pent_u_load */
    case 22: /* pent_v_load */
    case 23: /* pent_np_load */
    case 24: /* pent_uv */
    case 25: /* pent_u */
    case 26: /* pent_v */
    case 27: /* pent_np */

      temp = pentium_min_issue_delay [(pentium_translate [insn_code] + chip->pentium_automaton_state * 16) / 2];
      temp = (temp >> (8 - (pentium_translate [insn_code] % 2 + 1) * 4)) & 15;
      res = temp;
      break;

    case 2: /* pent_block */
    case 4: /* pent_fmov */
    case 5: /* pent_fpmovxf */
    case 6: /* pent_fpstore */
    case 12: /* pent_fp */
    case 13: /* pent_fmul */
    case 14: /* pent_fdiv */
    case 15: /* pent_fpspc */
    case 28: /* $advance_cycle */

      temp = pentium_fpu_min_issue_delay [pentium_fpu_translate [insn_code] + chip->pentium_fpu_automaton_state * 8];
      res = temp;

      temp = pentium_min_issue_delay [(pentium_translate [insn_code] + chip->pentium_automaton_state * 16) / 2];
      temp = (temp >> (8 - (pentium_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;
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
    case 0: /* pent_mul */
    case 1: /* pent_str */
    case 3: /* pent_cld */
    case 7: /* pent_imov */
    case 8: /* pent_push */
    case 9: /* pent_pop */
    case 10: /* pent_call */
    case 11: /* pent_branch */
    case 16: /* pent_uv_both */
    case 17: /* pent_u_both */
    case 18: /* pent_v_both */
    case 19: /* pent_np_both */
    case 20: /* pent_uv_load */
    case 21: /* pent_u_load */
    case 22: /* pent_v_load */
    case 23: /* pent_np_load */
    case 24: /* pent_uv */
    case 25: /* pent_u */
    case 26: /* pent_v */
    case 27: /* pent_np */
      {

        temp = pentium_base [chip->pentium_automaton_state] + pentium_translate [insn_code];
        if (pentium_check [temp] != chip->pentium_automaton_state)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->pentium_automaton_state = pentium_transitions [temp];
        return -1;
      }
    case 2: /* pent_block */
    case 4: /* pent_fmov */
    case 5: /* pent_fpmovxf */
    case 6: /* pent_fpstore */
    case 12: /* pent_fp */
    case 13: /* pent_fmul */
    case 14: /* pent_fdiv */
    case 15: /* pent_fpspc */
    case 28: /* $advance_cycle */
      {
        unsigned char _pentium_fpu_automaton_state;

        temp = pentium_fpu_base [chip->pentium_fpu_automaton_state] + pentium_fpu_translate [insn_code];
        if (pentium_fpu_check [temp] != chip->pentium_fpu_automaton_state)
          return internal_min_issue_delay (insn_code, chip);
        else
          _pentium_fpu_automaton_state = pentium_fpu_transitions [temp];

        temp = pentium_base [chip->pentium_automaton_state] + pentium_translate [insn_code];
        if (pentium_check [temp] != chip->pentium_automaton_state)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->pentium_automaton_state = pentium_transitions [temp];
        chip->pentium_fpu_automaton_state = _pentium_fpu_automaton_state;
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
    case 0: /* pent_mul */
    case 1: /* pent_str */
    case 3: /* pent_cld */
    case 7: /* pent_imov */
    case 8: /* pent_push */
    case 9: /* pent_pop */
    case 10: /* pent_call */
    case 11: /* pent_branch */
    case 16: /* pent_uv_both */
    case 17: /* pent_u_both */
    case 18: /* pent_v_both */
    case 19: /* pent_np_both */
    case 20: /* pent_uv_load */
    case 21: /* pent_u_load */
    case 22: /* pent_v_load */
    case 23: /* pent_np_load */
    case 24: /* pent_uv */
    case 25: /* pent_u */
    case 26: /* pent_v */
    case 27: /* pent_np */
      {
        int temp;

        temp = pentium_base_state_alts [chip->pentium_automaton_state] + pentium_translate [insn_code];
        if (pentium_check_state_alts [temp] != chip->pentium_automaton_state)
          return 0;
        else
          res = pentium_state_alts [temp];
        break;
      }

    case 2: /* pent_block */
    case 4: /* pent_fmov */
    case 5: /* pent_fpmovxf */
    case 6: /* pent_fpstore */
    case 12: /* pent_fp */
    case 13: /* pent_fmul */
    case 14: /* pent_fdiv */
    case 15: /* pent_fpspc */
    case 28: /* $advance_cycle */
      {
        int temp;

        temp = pentium_fpu_base_state_alts [chip->pentium_fpu_automaton_state] + pentium_fpu_translate [insn_code];
        if (pentium_fpu_check_state_alts [temp] != chip->pentium_fpu_automaton_state)
          return 0;
        else
          res = pentium_fpu_state_alts [temp];

        temp = pentium_base_state_alts [chip->pentium_automaton_state] + pentium_translate [insn_code];
        if (pentium_check_state_alts [temp] != chip->pentium_automaton_state)
          return 0;
        else
          res += pentium_state_alts [temp];
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
  if (pentium_dead_lock [chip->pentium_automaton_state])
    return 1/* TRUE */;
  if (pentium_fpu_dead_lock [chip->pentium_fpu_automaton_state])
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
      return (insn2_code != DFA__ADVANCE_CYCLE ? 11 : 0);
    case 1:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 12 : 0);
    case 2:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 3:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 4:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 5:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 6:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 7:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 8:
      switch (insn2_code)
        {
        case 10:
          return 0;
        case 9:
          return 0;
        case 8:
          return 0;
        default:
          return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
        }
    case 9:
      switch (insn2_code)
        {
        case 10:
          return 0;
        case 9:
          return 0;
        case 8:
          return 0;
        default:
          return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
        }
    case 10:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 10 : 0);
    case 11:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 12:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 13:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 14:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 39 : 0);
    case 15:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 70 : 0);
    case 16:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 17:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 18:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 19:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 20:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 21:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 22:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 23:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 24:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 25:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 26:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 27:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 28:
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
    case 0:
      fprintf (f, "pentium-np*11");
      break;
    case 1:
      fprintf (f, "pentium-np*12");
      break;
    case 2:
      fprintf (f, "(pentium-np+pentium-fp)");
      break;
    case 3:
      fprintf (f, "pentium-np*2");
      break;
    case 4:
      fprintf (f, "(pentium-fp+pentium-np)");
      break;
    case 5:
      fprintf (f, "((pentium-fp+pentium-np))*3");
      break;
    case 6:
      fprintf (f, "((pentium-fp+pentium-np))*2");
      break;
    case 7:
      fprintf (f, "pentium-firstuv");
      break;
    case 8:
      fprintf (f, "pentium-firstuv");
      break;
    case 9:
      fprintf (f, "pentium-firstuv");
      break;
    case 10:
      fprintf (f, "pentium-firstv,pentium-v*9");
      break;
    case 11:
      fprintf (f, "pentium-firstv");
      break;
    case 12:
      fprintf (f, "(pentium-firstu+pentium-fp),nothing,nothing");
      break;
    case 13:
      fprintf (f, "(pentium-firstuv+pentium-fp+pentium-fmul),pentium-fmul,nothing");
      break;
    case 14:
      fprintf (f, "(pentium-np+pentium-fp+pentium-fmul),((pentium-fp+pentium-fmul))*36,pentium-fmul*2");
      break;
    case 15:
      fprintf (f, "(pentium-np+pentium-fp+pentium-fmul),((pentium-fp+pentium-fmul))*67,pentium-fmul*2");
      break;
    case 16:
      fprintf (f, "pentium-firstuvboth,(pentium-uv+pentium-memory),pentium-uv");
      break;
    case 17:
      fprintf (f, "pentium-firstuboth,(pentium-u+pentium-memory),pentium-u");
      break;
    case 18:
      fprintf (f, "pentium-firstvboth,(pentium-v+pentium-memory),pentium-v");
      break;
    case 19:
      fprintf (f, "pentium-np,pentium-np,pentium-np");
      break;
    case 20:
      fprintf (f, "pentium-firstuvload,pentium-uv");
      break;
    case 21:
      fprintf (f, "pentium-firstuload,pentium-u");
      break;
    case 22:
      fprintf (f, "pentium-firstvload,pentium-v");
      break;
    case 23:
      fprintf (f, "pentium-np,pentium-np");
      break;
    case 24:
      fprintf (f, "pentium-firstuv");
      break;
    case 25:
      fprintf (f, "pentium-firstu");
      break;
    case 26:
      fprintf (f, "pentium-firstv");
      break;
    case 27:
      fprintf (f, "pentium-np");
      break;
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

int length_unit_log = 0;
