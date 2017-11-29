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
    case 519:
    case 513:
    case 511:
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) >= (-32768)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - (insn_current_reference_address (insn))) < (32764)))
        {
	  return 4;
        }
      else
        {
	  return 8;
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
    case 519:
    case 513:
    case 511:
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
    case 567:
    case 566:
    case 565:
    case 564:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 4;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 508:
    case 507:
    case 506:
    case 505:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 476:
    case 475:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 8;
        }
      else
        {
	  return 12 /* 0xc */;
        }

    case 480:
    case 479:
    case 473:
    case 472:
    case 462:
    case 461:
    case 459:
    case 438:
    case 437:
    case 435:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 424:
    case 423:
    case 421:
    case 420:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 1)
        {
	  return 8;
        }
      else if ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 5)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 6)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 425:
    case 422:
    case 419:
    case 418:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 1)
        {
	  return 8;
        }
      else
        {
	  return 12 /* 0xc */;
        }

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 20 /* 0x14 */;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11))))))
        {
	  return 4;
        }
      else
        {
	  return 4;
        }

    case 323:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 8;
        }
      else if ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 4;
        }
      else if (which_alternative == 6)
        {
	  return 8;
        }
      else if (which_alternative == 7)
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 8)
        {
	  return 8;
        }
      else if (which_alternative == 9)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 8;
        }
      else if (which_alternative == 3)
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 4)
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 20 /* 0x14 */;
        }

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 8;
        }
      else if (which_alternative == 6)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 309:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7))))))))
        {
	  return 4;
        }
      else if (which_alternative == 8)
        {
	  return 8;
        }
      else if (which_alternative == 9)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 308:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 8;
        }
      else if (which_alternative == 4)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 307:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 16 /* 0x10 */;
        }
      else if ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 4;
        }
      else if (which_alternative == 6)
        {
	  return 8;
        }
      else if (which_alternative == 7)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 306:
    case 305:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 10)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 8;
        }
      else if ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))
        {
	  return 4;
        }
      else
        {
	  return 4;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))))
        {
	  return 4;
        }
      else if (which_alternative == 6)
        {
	  return 8;
        }
      else if ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 4;
        }
      else
        {
	  return 4;
        }

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 4;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 8;
        }
      else
        {
	  return 12 /* 0xc */;
        }

    case 285:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 142:
    case 140:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 4;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 84:
    case 82:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else if ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 4;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 6)))
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 83:
    case 81:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 4;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 6)))
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 234:
    case 233:
    case 160:
    case 158:
    case 143:
    case 141:
    case 133:
    case 131:
    case 38:
    case 37:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 1073:
    case 1072:
    case 209:
      return 20 /* 0x14 */;

    case 759:
    case 758:
    case 757:
    case 756:
    case 755:
    case 754:
    case 753:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 493:
    case 488:
    case 487:
    case 485:
    case 482:
    case 481:
    case 478:
    case 477:
    case 471:
    case 470:
    case 469:
    case 466:
    case 464:
    case 460:
    case 458:
    case 457:
    case 454:
    case 452:
    case 449:
    case 448:
    case 441:
    case 440:
    case 439:
    case 436:
    case 434:
    case 416:
    case 414:
    case 413:
    case 412:
      return 12 /* 0xc */;

    case 545:
      return 0;

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
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if (which_alternative == 1)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 519:
    case 513:
    case 511:
    case 510:
    case 509:
    case 474:
    case 463:
    case 445:
    case 429:
    case 428:
    case 427:
    case 426:
    case 409:
    case 408:
    case 388:
    case 387:
    case 386:
    case 385:
    case 384:
    case 383:
    case 382:
    case 381:
    case 376:
    case 373:
    case 350:
    case 349:
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
    case 328:
    case 322:
    case 321:
    case 320:
    case 314:
    case 270:
    case 250:
    case 230:
    case 229:
    case 228:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 218:
    case 217:
    case 85:
    case 70:
      return 8;

    case 504:
    case 503:
    case 447:
    case 446:
    case 433:
    case 432:
    case 431:
    case 430:
    case 252:
    case 251:
    case 231:
    case 87:
    case 72:
    case 71:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else
        {
	  return 12 /* 0xc */;
        }

    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 486:
    case 484:
    case 483:
    case 468:
    case 467:
    case 465:
    case 456:
    case 455:
    case 453:
    case 451:
    case 450:
    case 444:
    case 443:
    case 442:
    case 417:
    case 415:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 402:
    case 401:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
    case 380:
    case 379:
    case 378:
    case 377:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 161:
    case 159:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 137:
    case 136:
    case 134:
    case 132:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 86:
    case 64:
    case 63:
    case 62:
    case 61:
    case 57:
    case 56:
    case 50:
    case 49:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
      return 32 /* 0x20 */;

    case 246:
    case 210:
    case 58:
      return 16 /* 0x10 */;

    case 208:
      return 24 /* 0x18 */;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 4;

    }
}

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
    case 703:
    case 702:
    case 701:
    case 700:
    case 694:
    case 693:
    case 692:
    case 674:
    case 673:
    case 666:
    case 665:
    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 651:
    case 650:
    case 649:
    case 648:
    case 647:
    case 636:
    case 629:
    case 628:
    case 627:
    case 626:
    case 625:
    case 624:
    case 620:
    case 605:
    case 598:
    case 597:
    case 593:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 714:
    case 713:
    case 712:
    case 711:
    case 710:
    case 709:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 699:
    case 690:
    case 689:
    case 688:
    case 687:
    case 686:
    case 678:
    case 644:
    case 643:
    case 642:
    case 641:
    case 640:
    case 639:
    case 638:
    case 637:
    case 623:
    case 622:
    case 621:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
    case 607:
    case 606:
    case 577:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 759:
    case 758:
    case 757:
    case 756:
    case 755:
    case 754:
    case 753:
    case 740:
    case 739:
    case 738:
    case 737:
    case 736:
    case 735:
    case 734:
    case 733:
    case 698:
    case 697:
    case 696:
    case 695:
    case 691:
    case 685:
    case 684:
    case 683:
    case 682:
    case 681:
    case 680:
    case 679:
    case 677:
    case 676:
    case 675:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 659:
    case 658:
    case 646:
    case 645:
    case 635:
    case 634:
    case 633:
    case 632:
    case 631:
    case 630:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 596:
    case 595:
    case 594:
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
      return 1;

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
    case 563:
    case 562:
    case 561:
    case 560:
    case 559:
    case 558:
    case 557:
    case 556:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 435:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if ((((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 4;
        }
      else if (((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if ((((rs6000_cpu_attr) == (CPU_POWER4))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 412:
    case 411:
    case 410:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 9;
        }
      else if ((((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620)))))))))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 367:
    case 363:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 328:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 3;
        }
      else if (((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if (((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 11) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))) || ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 5;
        }
      else if (((which_alternative == 11) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))))) || (((which_alternative == 7) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 4;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 11) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))) || (((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_PPC601)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 8) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((which_alternative == 11) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((which_alternative == 11) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((((which_alternative == 1) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((((which_alternative == 2) || (which_alternative == 9)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((which_alternative == 8) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  return 5;
        }
      else if (((which_alternative == 0) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  return 4;
        }
      else if (((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC601)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 310:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 3;
        }
      else if (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 3) && (which_alternative != 4)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 309:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 6) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))) || ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 5;
        }
      else if (((which_alternative == 6) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))))) || (((which_alternative == 3) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 4;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 6) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((which_alternative == 6) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((which_alternative == 6) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 308:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 1) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  return 5;
        }
      else if (((which_alternative == 3) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  return 4;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A))))))
        {
	  return 5;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))
        {
	  return 4;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))) || ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 5;
        }
      else if (((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))))) || (((which_alternative == 3) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))
        {
	  return 4;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A))))))
        {
	  return 5;
        }
      else if ((((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))))) || (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))))
        {
	  return 4;
        }
      else if (((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) || (((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_PPC7450))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 6) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A))))))
        {
	  return 5;
        }
      else if ((which_alternative == 6) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))
        {
	  return 4;
        }
      else if (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 6) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))))
        {
	  return 3;
        }
      else if (((which_alternative == 6) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((which_alternative == 6) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A))))))
        {
	  return 5;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))
        {
	  return 4;
        }
      else if (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))))
        {
	  return 3;
        }
      else if (((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if ((((rs6000_cpu_attr) == (CPU_POWER4))) || (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if ((((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 254:
    case 253:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 68 /* 0x44 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 66 /* 0x42 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  return 37 /* 0x25 */;
        }
      else
        {
	  return 1;
        }

    case 249:
    case 248:
    case 247:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 34 /* 0x22 */;
        }
      else if (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 7;
        }
      else
        {
	  return 1;
        }

    case 205:
    case 189:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 40 /* 0x28 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC620)))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  return 26 /* 0x1a */;
        }
      else
        {
	  return 1;
        }

    case 188:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 40 /* 0x28 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC620)))
        {
	  return 31 /* 0x1f */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 26 /* 0x1a */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  return 18 /* 0x12 */;
        }
      else
        {
	  return 1;
        }

    case 198:
    case 175:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 35 /* 0x23 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) || (((rs6000_cpu_attr) == (CPU_PPC603))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  return 31 /* 0x1f */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  return 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 1;
        }

    case 174:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 33 /* 0x21 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 31 /* 0x1f */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 19 /* 0x13 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620))))))
        {
	  return 18 /* 0x12 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  return 17 /* 0x11 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 1;
        }

    case 204:
    case 203:
    case 202:
    case 201:
    case 200:
    case 199:
    case 197:
    case 186:
    case 183:
    case 182:
    case 179:
    case 177:
    case 173:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 7;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 6;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7450))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 322:
    case 321:
    case 320:
    case 314:
    case 313:
    case 312:
    case 215:
    case 212:
    case 211:
    case 207:
    case 206:
    case 196:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 187:
    case 185:
    case 184:
    case 181:
    case 180:
    case 178:
    case 176:
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
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 5;
        }
      else if (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 87:
    case 86:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if (((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((((rs6000_cpu_attr) == (CPU_PPC7450))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_PPC405))))))
        {
	  return 4;
        }
      else if (((((rs6000_cpu_attr) == (CPU_MPCCORE))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 66 /* 0x42 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC603))))
        {
	  return 37 /* 0x25 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) || (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 36 /* 0x24 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC405)))
        {
	  return 35 /* 0x23 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC403)))
        {
	  return 33 /* 0x21 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 23 /* 0x17 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_PPC604E))))
        {
	  return 20 /* 0x14 */;
        }
      else if (((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 19 /* 0x13 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 13 /* 0xd */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 6;
        }
      else
        {
	  return 1;
        }

    case 60:
    case 59:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 20 /* 0x14 */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 12 /* 0xc */;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 8;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) && (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((rs6000_cpu_attr) == (CPU_POWER4))))) || (((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))
        {
	  return 5;
        }
      else if ((((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC604)))) || (((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC7450))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) || ((((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC403)))) || (((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RIOS1))))))))))
        {
	  return 4;
        }
      else if (((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || ((((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))))))
        {
	  return 3;
        }
      else if ((((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC604E))))) || (((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 486:
    case 484:
    case 483:
    case 453:
    case 417:
    case 415:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 161:
    case 160:
    case 159:
    case 158:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 143:
    case 142:
    case 141:
    case 140:
    case 137:
    case 136:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 64:
    case 63:
    case 62:
    case 61:
    case 50:
    case 49:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 508:
    case 507:
    case 506:
    case 505:
    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 480:
    case 479:
    case 476:
    case 475:
    case 473:
    case 472:
    case 468:
    case 467:
    case 465:
    case 462:
    case 461:
    case 459:
    case 456:
    case 455:
    case 451:
    case 450:
    case 447:
    case 446:
    case 444:
    case 443:
    case 442:
    case 438:
    case 437:
    case 433:
    case 432:
    case 431:
    case 430:
    case 424:
    case 423:
    case 421:
    case 420:
    case 407:
    case 406:
    case 405:
    case 404:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 252:
    case 251:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 234:
    case 233:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 72:
    case 71:
    case 57:
    case 56:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 38:
    case 37:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_POWER4))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_POWER4))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((which_alternative == 0) && (((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))))))
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
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    }
}

extern int lsu2_unit_ready_cost PARAMS ((rtx));
int
lsu2_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if (((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4)))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if (((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4)))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 328:
    case 308:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 5;

    }
}

extern int cru_unit_ready_cost PARAMS ((rtx));
int
cru_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E))))))
        {
	  return 1;
        }
      else
        {
	  return 4;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC630)))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E))))))
        {
	  return 1;
        }
      else
        {
	  return 4;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 4;

    }
}

extern int bpu_unit_ready_cost PARAMS ((rtx));
int
bpu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC604)))))
        {
	  return 4;
        }
      else
        {
	  return 5;
        }

    case 550:
    case 549:
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
    case 524:
    case 523:
    case 522:
    case 521:
    case 520:
    case 519:
    case 514:
    case 513:
    case 512:
    case 511:
    case 402:
    case 401:
    case 400:
    case 399:
    case 398:
    case 397:
    case 396:
    case 395:
    case 394:
    case 393:
    case 392:
    case 391:
    case 390:
    case 389:
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
    case 378:
    case 377:
    case 375:
    case 373:
    case 372:
    case 371:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))))))))))
        {
	  return 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC8540))) || ((((rs6000_cpu_attr) == (CPU_PPC7450))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750))))))
        {
	  return 1;
        }
      else
        {
	  return 5;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 11)) || (((((rs6000_cpu_attr) == (CPU_PPC630))) && (which_alternative == 11)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 11)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 11)) || (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 11)) || (((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 11)) || (((((rs6000_cpu_attr) == (CPU_PPC405))) && (which_alternative == 11)) || (((((rs6000_cpu_attr) == (CPU_PPC403))) && (which_alternative == 11)) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 11))))))))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) && (which_alternative == 11))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 1)) || (((((rs6000_cpu_attr) == (CPU_PPC630))) && (which_alternative == 1)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 1)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 1)) || (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 1)) || (((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 1)) || (((((rs6000_cpu_attr) == (CPU_PPC405))) && (which_alternative == 1)) || (((((rs6000_cpu_attr) == (CPU_PPC403))) && (which_alternative == 1)) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 1))))))))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) && (which_alternative == 1))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case 305:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC630))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC405))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC403))) && (which_alternative == 7)) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 7))))))))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) && (which_alternative == 7))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_RIOS2))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))))) || (((((rs6000_cpu_attr) == (CPU_RIOS1))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))))) || (((((rs6000_cpu_attr) == (CPU_PPC604))) && ((which_alternative == 7) || ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))))) || (((((rs6000_cpu_attr) == (CPU_PPC630))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC405))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC403))) && (which_alternative == 7)) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 7))))))))))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) && (which_alternative == 7))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case 309:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 6)) || (((((rs6000_cpu_attr) == (CPU_PPC630))) && (which_alternative == 6)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 6)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 6)) || (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 6)) || (((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 6)) || (((((rs6000_cpu_attr) == (CPU_PPC405))) && (which_alternative == 6)) || (((((rs6000_cpu_attr) == (CPU_PPC403))) && (which_alternative == 6)) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 6))))))))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) && (which_alternative == 6))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 10)) || (((((rs6000_cpu_attr) == (CPU_PPC630))) && (which_alternative == 10)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 10)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 10)) || (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 10)) || (((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 10)) || (((((rs6000_cpu_attr) == (CPU_PPC405))) && (which_alternative == 10)) || (((((rs6000_cpu_attr) == (CPU_PPC403))) && (which_alternative == 10)) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 10))))))))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_POWER4))) && (which_alternative == 10))
        {
	  return 3;
        }
      else
        {
	  return 5;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 5;

    }
}

extern int fpu2_unit_ready_cost PARAMS ((rtx));
int
fpu2_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 412:
    case 411:
    case 410:
      if ((((rs6000_cpu_attr) == (CPU_POWER4))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 5;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  return 3;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  return 3;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 6;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  return 3;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 205:
    case 189:
      if ((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 26 /* 0x1a */;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 188:
      if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  return 18 /* 0x12 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 26 /* 0x1a */;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 198:
    case 175:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 33 /* 0x21 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  return 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 174:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case 322:
    case 321:
    case 320:
    case 314:
    case 313:
    case 312:
    case 215:
    case 212:
    case 211:
    case 207:
    case 206:
    case 204:
    case 203:
    case 202:
    case 201:
    case 200:
    case 199:
    case 197:
    case 196:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 187:
    case 186:
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
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  return 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 2;
        }
      else
        {
	  return 40 /* 0x28 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 40 /* 0x28 */;

    }
}

extern unsigned int fpu2_unit_blockage_range PARAMS ((rtx));
unsigned int
fpu2_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65571 /* min 1, max 35 */;

    }
}

extern int sru_unit_ready_cost PARAMS ((rtx));
int
sru_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 324:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 11)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 11)))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 1)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 1)))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 305:
    case 304:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 7)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 7)))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 309:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 6)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 6)))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 10)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 10)))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 3;

    }
}

extern int vec_alu2_unit_ready_cost PARAMS ((rtx));
int
vec_alu2_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
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
    case 703:
    case 702:
    case 701:
    case 700:
    case 694:
    case 693:
    case 692:
    case 674:
    case 673:
    case 666:
    case 665:
    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 651:
    case 650:
    case 649:
    case 648:
    case 647:
    case 636:
    case 629:
    case 628:
    case 627:
    case 626:
    case 625:
    case 624:
    case 620:
    case 605:
    case 598:
    case 597:
    case 593:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 740:
    case 739:
    case 738:
    case 737:
    case 736:
    case 735:
    case 734:
    case 733:
    case 698:
    case 697:
    case 696:
    case 695:
    case 691:
    case 685:
    case 684:
    case 683:
    case 682:
    case 681:
    case 680:
    case 679:
    case 677:
    case 676:
    case 675:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 659:
    case 658:
    case 646:
    case 645:
    case 635:
    case 634:
    case 633:
    case 632:
    case 631:
    case 630:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 596:
    case 595:
    case 594:
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
    case 576:
    case 575:
    case 574:
    case 573:
    case 572:
    case 571:
    case 570:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 1;
        }
      else
        {
	  return 4;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 4;

    }
}

extern unsigned int vec_alu2_unit_blockage_range PARAMS ((rtx));
unsigned int
vec_alu2_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
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
    case 714:
    case 713:
    case 712:
    case 711:
    case 710:
    case 709:
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
    case 659:
    case 658:
    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 651:
    case 650:
    case 649:
    case 648:
    case 647:
    case 646:
    case 645:
    case 644:
    case 643:
    case 642:
    case 641:
    case 640:
    case 639:
    case 638:
    case 637:
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
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 2 /* min 0, max 2 */;
        }
      else
        {
	  return 65538 /* min 1, max 2 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65538 /* min 1, max 2 */;

    }
}

extern int iu3_unit_ready_cost PARAMS ((rtx));
int
iu3_unit_ready_cost (insn)
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

extern int imuldiv_unit_ready_cost PARAMS ((rtx));
int
imuldiv_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 11))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 1))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 305:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 7))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 7))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 309:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 6))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 10))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 254:
    case 253:
      if ((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620))))
        {
	  return 37 /* 0x25 */;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 249:
    case 248:
    case 247:
      if ((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620))))
        {
	  return 7;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if ((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620))))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC604E)))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 36 /* 0x24 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  return 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC620)))
        {
	  return 37 /* 0x25 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 23 /* 0x17 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  return 19 /* 0x13 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 13 /* 0xd */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 6;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 60:
    case 59:
      extract_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC630))) && (get_attr_type (insn) == TYPE_IMUL))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) && (get_attr_type (insn) == TYPE_IMUL2))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) && (s8bit_cint_operand (operands[2], SImode)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) && (get_attr_type (insn) == TYPE_IMUL))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) && (get_attr_type (insn) == TYPE_IMUL2))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) && (s8bit_cint_operand (operands[2], SImode)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (get_attr_type (insn) == TYPE_IMUL))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604E))) && (((get_attr_type (insn) == TYPE_IMUL) || (get_attr_type (insn) == TYPE_IMUL2)) || (s8bit_cint_operand (operands[2], SImode))))
        {
	  return 2;
        }
      else if (((((rs6000_cpu_attr) == (CPU_PPC604))) && (((get_attr_type (insn) == TYPE_IMUL) || (get_attr_type (insn) == TYPE_IMUL2)) || (s8bit_cint_operand (operands[2], SImode)))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) && (get_attr_type (insn) == TYPE_IMUL)))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) && (get_attr_type (insn) == TYPE_IMUL2))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) && (s8bit_cint_operand (operands[2], SImode)))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) && (get_attr_type (insn) == TYPE_IMUL))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) && (get_attr_type (insn) == TYPE_IMUL2))
        {
	  return 3;
        }
      else if (((((rs6000_cpu_attr) == (CPU_PPC750))) && (s8bit_cint_operand (operands[2], SImode))) || (((((rs6000_cpu_attr) == (CPU_RIOS2))) && (((get_attr_type (insn) == TYPE_IMUL) || (get_attr_type (insn) == TYPE_IMUL2)) || (s8bit_cint_operand (operands[2], SImode)))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (((get_attr_type (insn) == TYPE_IMUL) || (get_attr_type (insn) == TYPE_IMUL2)) || (s8bit_cint_operand (operands[2], SImode))))))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 68 /* 0x44 */;

    }
}

extern unsigned int imuldiv_unit_blockage_range PARAMS ((rtx));
unsigned int
imuldiv_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65603 /* min 1, max 67 */;

    }
}

extern int iu2_unit_ready_cost PARAMS ((rtx));
int
iu2_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 435:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 4;
        }
      else if (((which_alternative != 0) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if (((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if (((which_alternative != 0) && (which_alternative != 2)) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 328:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if ((which_alternative == 0) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if (((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 2;
        }
      else if (((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 310:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 3) && (which_alternative != 4)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 309:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 2;
        }
      else if (((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 308:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 1) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 2;
        }
      else if (((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 305:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 2;
        }
      else if (((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 555:
    case 374:
    case 370:
    case 369:
    case 367:
    case 365:
    case 363:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 4;
        }
      else if (((which_alternative == 1) || (which_alternative == 4)) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) || (which_alternative == 4)) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 249:
    case 248:
    case 247:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 7;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 87:
    case 86:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 4;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if ((which_alternative == 0) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 4;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if ((which_alternative == 3) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 3;
        }
      else if ((which_alternative != 3) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 1;
        }
      else if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 3;
        }
      else if ((which_alternative != 3) && (((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  return 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  return 19 /* 0x13 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 13 /* 0xd */;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 60:
    case 59:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 4;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 4;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || (((rs6000_cpu_attr) == (CPU_RIOS2)))))
        {
	  return 2;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 4;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 3;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 486:
    case 484:
    case 483:
    case 453:
    case 417:
    case 415:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 161:
    case 160:
    case 159:
    case 158:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 143:
    case 142:
    case 141:
    case 140:
    case 137:
    case 136:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 64:
    case 63:
    case 62:
    case 61:
    case 50:
    case 49:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  return 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 508:
    case 507:
    case 506:
    case 505:
    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 480:
    case 479:
    case 476:
    case 475:
    case 473:
    case 472:
    case 468:
    case 467:
    case 465:
    case 462:
    case 461:
    case 459:
    case 456:
    case 455:
    case 451:
    case 450:
    case 447:
    case 446:
    case 444:
    case 443:
    case 442:
    case 438:
    case 437:
    case 433:
    case 432:
    case 431:
    case 430:
    case 424:
    case 423:
    case 421:
    case 420:
    case 407:
    case 406:
    case 405:
    case 404:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 252:
    case 251:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 234:
    case 233:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 72:
    case 71:
    case 57:
    case 56:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 38:
    case 37:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  return 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  return 2;
        }
      else if ((which_alternative == 1) && ((((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 1;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  return 2;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1016:
    case 1015:
    case 554:
    case 553:
    case 552:
    case 551:
    case 548:
    case 545:
    case 544:
    case 543:
    case 542:
    case 540:
    case 539:
    case 538:
    case 525:
    case 518:
    case 510:
    case 509:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 493:
    case 488:
    case 487:
    case 485:
    case 482:
    case 481:
    case 478:
    case 477:
    case 474:
    case 471:
    case 470:
    case 469:
    case 466:
    case 464:
    case 463:
    case 460:
    case 458:
    case 457:
    case 454:
    case 452:
    case 449:
    case 448:
    case 445:
    case 441:
    case 440:
    case 439:
    case 436:
    case 434:
    case 429:
    case 428:
    case 427:
    case 426:
    case 425:
    case 422:
    case 419:
    case 418:
    case 409:
    case 408:
    case 403:
    case 376:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 298:
    case 297:
    case 294:
    case 291:
    case 288:
    case 285:
    case 282:
    case 279:
    case 276:
    case 273:
    case 270:
    case 267:
    case 264:
    case 261:
    case 258:
    case 255:
    case 250:
    case 246:
    case 243:
    case 242:
    case 241:
    case 238:
    case 235:
    case 232:
    case 231:
    case 230:
    case 229:
    case 228:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 210:
    case 209:
    case 208:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 147:
    case 144:
    case 139:
    case 138:
    case 135:
    case 130:
    case 129:
    case 126:
    case 123:
    case 120:
    case 117:
    case 116:
    case 115:
    case 114:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 105:
    case 100:
    case 99:
    case 98:
    case 97:
    case 94:
    case 91:
    case 88:
    case 85:
    case 80:
    case 70:
    case 58:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 48:
    case 43:
    case 42:
    case 39:
    case 36:
    case 27:
    case 21:
    case 3:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604)))))))))
        {
	  return 1;
        }
      else
        {
	  return 68 /* 0x44 */;
        }

    default:
      return 68 /* 0x44 */;

    }
}

extern unsigned int iu2_unit_blockage_range PARAMS ((rtx));
unsigned int
iu2_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65603 /* min 1, max 67 */;

    }
}

extern int fpu_unit_ready_cost PARAMS ((rtx));
int
fpu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 412:
    case 411:
    case 410:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC620))))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7400)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC750)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 9;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 1;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 368:
    case 364:
      if ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 1;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 7))
        {
	  return 5;
        }
      else if (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 7)) || (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 7)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 7)))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 7))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 9))
        {
	  return 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) && (which_alternative == 7))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS1))) && (which_alternative == 7))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS1))) && (which_alternative == 9))
        {
	  return 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 7))
        {
	  return 4;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 0))
        {
	  return 5;
        }
      else if (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 0)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 0)) || (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 0)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 0)) || (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 0)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 0)))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 2))
        {
	  return 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS1))) && (which_alternative == 0))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS1))) && (which_alternative == 2))
        {
	  return 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 0))
        {
	  return 4;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_PPC7450))) && (which_alternative == 3))
        {
	  return 5;
        }
      else if (((((rs6000_cpu_attr) == (CPU_PPC603))) && (which_alternative == 3)) || (((((rs6000_cpu_attr) == (CPU_PPC620))) && (which_alternative == 3)) || (((((rs6000_cpu_attr) == (CPU_PPC7400))) && (which_alternative == 3)) || (((((rs6000_cpu_attr) == (CPU_PPC604E))) && (which_alternative == 3)) || (((((rs6000_cpu_attr) == (CPU_PPC604))) && (which_alternative == 3)) || ((((rs6000_cpu_attr) == (CPU_PPC750))) && (which_alternative == 3)))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 3))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) && (which_alternative == 5))
        {
	  return 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) && (which_alternative == 3))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS1))) && (which_alternative == 3))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RIOS1))) && (which_alternative == 5))
        {
	  return 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_MPCCORE))) && (which_alternative == 3))
        {
	  return 4;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 205:
    case 189:
    case 188:
      if (((rs6000_cpu_attr) == (CPU_PPC620)))
        {
	  return 31 /* 0x1f */;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 198:
    case 175:
      if (((rs6000_cpu_attr) == (CPU_PPC603)))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))))))
        {
	  return 31 /* 0x1f */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 19 /* 0x13 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 174:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC620))))
        {
	  return 18 /* 0x12 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7400)))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))
        {
	  return 18 /* 0x12 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 17 /* 0x11 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 31 /* 0x1f */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 19 /* 0x13 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 204:
    case 203:
    case 202:
    case 201:
    case 200:
    case 199:
    case 197:
    case 186:
    case 183:
    case 182:
    case 179:
    case 177:
    case 173:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC603)))
        {
	  return 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))))
        {
	  return 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC750)))
        {
	  return 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 7;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 2;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 5;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 322:
    case 321:
    case 320:
    case 314:
    case 313:
    case 312:
    case 215:
    case 212:
    case 211:
    case 207:
    case 206:
    case 196:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 187:
    case 185:
    case 184:
    case 181:
    case 180:
    case 178:
    case 176:
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
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_PPC750))))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 2;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 4;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 35 /* 0x23 */;

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
      return 65571 /* min 1, max 35 */;

    }
}

extern int iu_unit_ready_cost PARAMS ((rtx));
int
iu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 435:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && (which_alternative != 2)) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))))
        {
	  return 3;
        }
      else if (((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if ((((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((which_alternative == 0) || (which_alternative == 2)) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 4;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  return 3;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 412:
    case 411:
    case 410:
      if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 367:
    case 363:
      if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  return 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 2;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 328:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if (((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || (((which_alternative == 0) || (which_alternative == 1)) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 3;
        }
      else if (((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))
        {
	  return 2;
        }
      else if ((((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || (((which_alternative == 9) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 2;
        }
      else if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if ((((which_alternative != 3) && (which_alternative != 4)) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || ((which_alternative == 4) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 309:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 3;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))
        {
	  return 2;
        }
      else if ((((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || (((which_alternative == 5) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 308:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if ((((which_alternative != 1) && (which_alternative != 2)) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 3;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))
        {
	  return 2;
        }
      else if ((((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || (((which_alternative == 5) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if ((((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || ((which_alternative == 5) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 3;
        }
      else if (((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))
        {
	  return 2;
        }
      else if ((((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || (((which_alternative == 5) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if ((((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))) || ((((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || (((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))) || ((which_alternative == 10) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if ((((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if ((((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))) || ((which_alternative == 3) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1))))))
        {
	  return 2;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if ((((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 4;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  return 3;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 249:
    case 248:
    case 247:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 34 /* 0x22 */;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 87:
    case 86:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))))
        {
	  return 3;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if (((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 4;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  return 3;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))))
        {
	  return 3;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if (((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || ((which_alternative != 3) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))))
        {
	  return 3;
        }
      else if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 4;
        }
      else if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  return 3;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  return 20 /* 0x14 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  return 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))
        {
	  return 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 5;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_PPC603)))
        {
	  return 37 /* 0x25 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  return 36 /* 0x24 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC405)))
        {
	  return 35 /* 0x23 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC403)))
        {
	  return 33 /* 0x21 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 19 /* 0x13 */;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 60:
    case 59:
      extract_insn_cached (insn);
      if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 20 /* 0x14 */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))
        {
	  return 4;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 12 /* 0xc */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  return 5;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_PPC405))))
        {
	  return 3;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 4;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  return 8;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  return 5;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_PPC405))))
        {
	  return 3;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_PPC403))))
        {
	  return 4;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  return 3;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 486:
    case 484:
    case 483:
    case 453:
    case 417:
    case 415:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 161:
    case 160:
    case 159:
    case 158:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 143:
    case 142:
    case 141:
    case 140:
    case 137:
    case 136:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 64:
    case 63:
    case 62:
    case 61:
    case 50:
    case 49:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))))
        {
	  return 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 3;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 508:
    case 507:
    case 506:
    case 505:
    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 480:
    case 479:
    case 476:
    case 475:
    case 473:
    case 472:
    case 468:
    case 467:
    case 465:
    case 462:
    case 461:
    case 459:
    case 456:
    case 455:
    case 451:
    case 450:
    case 447:
    case 446:
    case 444:
    case 443:
    case 442:
    case 438:
    case 437:
    case 433:
    case 432:
    case 431:
    case 430:
    case 424:
    case 423:
    case 421:
    case 420:
    case 407:
    case 406:
    case 405:
    case 404:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 252:
    case 251:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 234:
    case 233:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 72:
    case 71:
    case 57:
    case 56:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 38:
    case 37:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))))
        {
	  return 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  return 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  return 3;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403))))) || (((rs6000_cpu_attr) == (CPU_RIOS1)))))
        {
	  return 2;
        }
      else if ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC403)))))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1016:
    case 1015:
    case 554:
    case 553:
    case 552:
    case 551:
    case 548:
    case 545:
    case 544:
    case 543:
    case 542:
    case 540:
    case 539:
    case 538:
    case 525:
    case 518:
    case 510:
    case 509:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 493:
    case 488:
    case 487:
    case 485:
    case 482:
    case 481:
    case 478:
    case 477:
    case 474:
    case 471:
    case 470:
    case 469:
    case 466:
    case 464:
    case 463:
    case 460:
    case 458:
    case 457:
    case 454:
    case 452:
    case 449:
    case 448:
    case 445:
    case 441:
    case 440:
    case 439:
    case 436:
    case 434:
    case 429:
    case 428:
    case 427:
    case 426:
    case 425:
    case 422:
    case 419:
    case 418:
    case 409:
    case 408:
    case 403:
    case 376:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 298:
    case 297:
    case 294:
    case 291:
    case 288:
    case 285:
    case 282:
    case 279:
    case 276:
    case 273:
    case 270:
    case 267:
    case 264:
    case 261:
    case 258:
    case 255:
    case 250:
    case 246:
    case 243:
    case 242:
    case 241:
    case 238:
    case 235:
    case 232:
    case 231:
    case 230:
    case 229:
    case 228:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 210:
    case 209:
    case 208:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 147:
    case 144:
    case 139:
    case 138:
    case 135:
    case 130:
    case 129:
    case 126:
    case 123:
    case 120:
    case 117:
    case 116:
    case 115:
    case 114:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 105:
    case 100:
    case 99:
    case 98:
    case 97:
    case 94:
    case 91:
    case 88:
    case 85:
    case 80:
    case 70:
    case 58:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 48:
    case 43:
    case 42:
    case 39:
    case 36:
    case 27:
    case 21:
    case 3:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))))))
        {
	  return 1;
        }
      else
        {
	  return 66 /* 0x42 */;
        }

    default:
      return 66 /* 0x42 */;

    }
}

extern unsigned int iu_unit_blockage_range PARAMS ((rtx));
unsigned int
iu_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65602 /* min 1, max 66 */;

    }
}

extern int lsu_unit_ready_cost PARAMS ((rtx));
int
lsu_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
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
    case 563:
    case 562:
    case 561:
    case 560:
    case 559:
    case 558:
    case 557:
    case 556:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 3;
        }
      else
        {
	  return 4;
        }

    case 367:
    case 363:
      if ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))))
        {
	  return 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  return 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  return 1;
        }
      else
        {
	  return 4;
        }

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 8) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 3;
        }
      else if ((which_alternative == 8) && (((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))
        {
	  return 2;
        }
      else if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 9) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 2) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && (((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 2) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else
        {
	  return 4;
        }

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 4) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 3) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 5) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 4) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 3;
        }
      else if ((which_alternative == 4) && (((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))))
        {
	  return 2;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 5) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 2) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 10) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 9) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 328:
    case 308:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 2) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 1) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 3) && ((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))
        {
	  return 1;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC750)))))
        {
	  return 2;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 1;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 2) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  return 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  return 3;
        }
      else if ((which_alternative == 0) && (((((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC604))))) || (((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 4;

    }
}

extern unsigned int lsu_unit_blockage_range PARAMS ((rtx));
unsigned int
lsu_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65539 /* min 1, max 3 */;

    }
}

extern int function_units_used PARAMS ((rtx));
int
function_units_used (insn)
     rtx insn;
{
  enum attr_type attr_type = get_attr_type (insn);
  unsigned long accum = 0;

  accum |= ((((attr_type == TYPE_LOAD) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))) || ((((attr_type == TYPE_LOAD) || (attr_type == TYPE_VECLOAD)) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((((attr_type == TYPE_STORE) || (attr_type == TYPE_FPSTORE)) && (((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) || ((((attr_type == TYPE_STORE) || (attr_type == TYPE_VECSTORE)) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_FPSTORE) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((attr_type == TYPE_FPLOAD) && ((((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))))))) ? (1) : (0));
  accum |= ((((attr_type == TYPE_LOAD) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || ((((attr_type == TYPE_STORE) || (attr_type == TYPE_FPSTORE)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || (((attr_type == TYPE_FPLOAD) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || (((attr_type == TYPE_INTEGER) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))))) || (((attr_type == TYPE_CR_LOGICAL) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))) || ((((attr_type == TYPE_IMUL) || ((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3))) && (((rs6000_cpu_attr) == (CPU_PPC403)))) || (((attr_type == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_PPC405)))) || ((((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3)) && (((rs6000_cpu_attr) == (CPU_PPC405)))) || (((attr_type == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((attr_type == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((attr_type == TYPE_IMUL3) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || ((((attr_type == TYPE_IMUL) || ((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3))) && ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))) || (((attr_type == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RS64A)))) || (((attr_type == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RS64A)))) || (((attr_type == TYPE_IMUL3) && (((rs6000_cpu_attr) == (CPU_RS64A)))) || (((attr_type == TYPE_LMUL) && (((rs6000_cpu_attr) == (CPU_RS64A)))) || (((attr_type == TYPE_IDIV) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_RS64A))))) || (((attr_type == TYPE_LDIV) && (((rs6000_cpu_attr) == (CPU_RS64A)))) || (((attr_type == TYPE_IDIV) && ((((((rs6000_cpu_attr) == (CPU_PPC403))) || (((rs6000_cpu_attr) == (CPU_PPC405)))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || (((rs6000_cpu_attr) == (CPU_PPC603))))) || (((attr_type == TYPE_COMPARE) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((attr_type == TYPE_DELAYED_COMPARE) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || ((((attr_type == TYPE_COMPARE) || (attr_type == TYPE_DELAYED_COMPARE)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))) || ((attr_type == TYPE_FPCOMPARE) && ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))))))))))))))))))) ? (2) : (0));
  accum |= ((((attr_type == TYPE_FPSTORE) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601))))) || (((attr_type == TYPE_FPCOMPARE) && ((((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620))))))))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))) || ((((attr_type == TYPE_FP) || (attr_type == TYPE_DMUL)) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((attr_type == TYPE_FP) && ((((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))) || ((((attr_type == TYPE_FP) || (attr_type == TYPE_DMUL)) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_DMUL) && (((((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((rs6000_cpu_attr) == (CPU_PPC601)))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC750))))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))) || ((((attr_type == TYPE_SDIV) || (attr_type == TYPE_DDIV)) && (((rs6000_cpu_attr) == (CPU_RIOS1)))) || (((attr_type == TYPE_SDIV) && (((((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620)))))))) || (((attr_type == TYPE_DDIV) && ((((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((rs6000_cpu_attr) == (CPU_PPC603))))) || (((attr_type == TYPE_SSQRT) && (((rs6000_cpu_attr) == (CPU_PPC620)))) || ((attr_type == TYPE_DSQRT) && (((rs6000_cpu_attr) == (CPU_PPC620)))))))))))))) ? (4) : (0));
  accum |= (((((attr_type == TYPE_LOAD) || (attr_type == TYPE_FPLOAD)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((attr_type == TYPE_STORE) || (attr_type == TYPE_FPSTORE)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((attr_type == TYPE_INTEGER) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((attr_type == TYPE_IMUL) || ((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3))) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((attr_type == TYPE_IDIV) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((attr_type == TYPE_INTEGER) && (((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) || (((attr_type == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((attr_type == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((attr_type == TYPE_IMUL3) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((attr_type == TYPE_IDIV) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || ((((attr_type == TYPE_COMPARE) || (attr_type == TYPE_DELAYED_COMPARE)) && ((((rs6000_cpu_attr) == (CPU_RIOS2))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))) || (((attr_type == TYPE_INTEGER) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_LMUL) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_IDIV) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_LDIV) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_COMPARE) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((attr_type == TYPE_DELAYED_COMPARE) && (((rs6000_cpu_attr) == (CPU_POWER4)))))))))))))))))))))) ? (8) : (0));
  accum |= (((((attr_type == TYPE_IMUL) || ((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3))) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((attr_type == TYPE_IDIV) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || ((((attr_type == TYPE_IMUL) || ((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3))) && (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || (((attr_type == TYPE_IDIV) && (((rs6000_cpu_attr) == (CPU_MPCCORE)))) || ((((attr_type == TYPE_IMUL) || ((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_PPC604E))))) || (((attr_type == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((attr_type == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((attr_type == TYPE_IMUL3) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((attr_type == TYPE_LMUL) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((attr_type == TYPE_IDIV) && ((((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_PPC604E)))) || (((rs6000_cpu_attr) == (CPU_PPC620)))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((attr_type == TYPE_LDIV) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || (((attr_type == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((((attr_type == TYPE_IMUL2) || (attr_type == TYPE_IMUL3)) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_IDIV) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_CR_LOGICAL) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((attr_type == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((attr_type == TYPE_IMUL3) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((attr_type == TYPE_IDIV) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))) || (((attr_type == TYPE_MTJMPR) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_IDIV) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((attr_type == TYPE_LDIV) && (((rs6000_cpu_attr) == (CPU_POWER4))))))))))))))))))))))))) ? (16) : (0));
  accum |= ((((attr_type == TYPE_INTEGER) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_COMPARE) || (attr_type == TYPE_DELAYED_COMPARE)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))) ? (32) : (0));
  accum |= ((((attr_type == TYPE_VECSIMPLE) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_VECCOMPLEX) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_VECCMP) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || (((attr_type == TYPE_VECFLOAT) && (((rs6000_cpu_attr) == (CPU_PPC7450)))) || ((attr_type == TYPE_VECPERM) && (((rs6000_cpu_attr) == (CPU_PPC7450)))))))) ? (64) : (0));
  accum |= ((((attr_type == TYPE_CR_LOGICAL) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) || ((attr_type == TYPE_MTJMPR) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))) ? (128) : (0));
  accum |= ((((attr_type == TYPE_FPCOMPARE) && ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || ((((attr_type == TYPE_FP) || (attr_type == TYPE_DMUL)) && ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))) || ((((attr_type == TYPE_SDIV) || (attr_type == TYPE_DDIV)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((attr_type == TYPE_SDIV) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((attr_type == TYPE_DDIV) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || ((((attr_type == TYPE_SSQRT) || (attr_type == TYPE_DSQRT)) && (((rs6000_cpu_attr) == (CPU_RIOS2)))) || (((attr_type == TYPE_SSQRT) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || (((attr_type == TYPE_DSQRT) && (((rs6000_cpu_attr) == (CPU_PPC630)))) || ((((attr_type == TYPE_FP) || (attr_type == TYPE_DMUL)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_FPCOMPARE) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || ((((attr_type == TYPE_SDIV) || (attr_type == TYPE_DDIV)) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_SSQRT) || (attr_type == TYPE_DSQRT)) && (((rs6000_cpu_attr) == (CPU_POWER4))))))))))))))) ? (256) : (0));
  accum |= ((((attr_type == TYPE_MTJMPR) && (((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_RS64A))))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))))) || (((attr_type == TYPE_CR_LOGICAL) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC604)))))) || ((attr_type == TYPE_JMPREG) || ((attr_type == TYPE_BRANCH) || (((attr_type == TYPE_MTJMPR) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_JMPREG) || (attr_type == TYPE_BRANCH)) && (((rs6000_cpu_attr) == (CPU_POWER4))))))))) ? (512) : (0));
  accum |= ((((attr_type == TYPE_LOAD) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_FPLOAD) && (((rs6000_cpu_attr) == (CPU_POWER4)))) || (((attr_type == TYPE_STORE) || (attr_type == TYPE_FPSTORE)) && (((rs6000_cpu_attr) == (CPU_POWER4)))))) ? (2048) : (0));
  accum |= (((attr_type == TYPE_CR_LOGICAL) && (((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || (((rs6000_cpu_attr) == (CPU_RS64A)))))) || (((rs6000_cpu_attr) == (CPU_POWER4))))) ? (1024) : (0));

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

extern enum attr_type get_attr_type PARAMS ((rtx));
enum attr_type
get_attr_type (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_INTEGER;
        }
      else
        {
	  return TYPE_LOAD;
        }

    case 324:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_FP;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_FPLOAD;
        }
      else if (which_alternative == 9)
        {
	  return TYPE_FPSTORE;
        }
      else if (which_alternative == 10)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_MTJMPR;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 311:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_FP;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_FPLOAD;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_FPSTORE;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 309:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_FP;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_FPLOAD;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_FPSTORE;
        }
      else if (which_alternative == 6)
        {
	  return TYPE_MTJMPR;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 308:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_FP;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_FPLOAD;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_FPSTORE;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 306:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_MTJMPR;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 305:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_FP;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_FPLOAD;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_FPSTORE;
        }
      else if (which_alternative == 6)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_MTJMPR;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return TYPE_CR_LOGICAL;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_MTJMPR;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 9)
        {
	  return TYPE_LOAD;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 6)
        {
	  return TYPE_MTJMPR;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 10)
        {
	  return TYPE_MTJMPR;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_COMPARE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_DELAYED_COMPARE;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_COMPARE;
        }
      else if (which_alternative == 4)
        {
	  return TYPE_DELAYED_COMPARE;
        }
      else if ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return TYPE_COMPARE;
        }
      else
        {
	  return TYPE_COMPARE;
        }

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_COMPARE;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_DELAYED_COMPARE;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 6)))
        {
	  return TYPE_COMPARE;
        }
      else
        {
	  return TYPE_COMPARE;
        }

    case 0:
    case 6:
    case 9:
    case 12:
    case 15:
    case 18:
    case 24:
    case 30:
    case 33:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_LOAD;
        }
      else
        {
	  return TYPE_INTEGER;
        }

    case 59:
    case 60:
      extract_insn_cached (insn);
      if (s8bit_cint_operand (operands[2], SImode))
        {
	  return TYPE_IMUL3;
        }
      else if (short_cint_operand (operands[2], SImode))
        {
	  return TYPE_IMUL2;
        }
      else
        {
	  return TYPE_IMUL;
        }

    case 86:
    case 87:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_DELAYED_COMPARE;
        }
      else
        {
	  return TYPE_COMPARE;
        }

    case 328:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_INTEGER;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 435:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_COMPARE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_DELAYED_COMPARE;
        }
      else if (which_alternative == 2)
        {
	  return TYPE_COMPARE;
        }
      else
        {
	  return TYPE_DELAYED_COMPARE;
        }

    case 564:
    case 565:
    case 566:
    case 567:
    case 568:
    case 569:
    case 753:
    case 754:
    case 755:
    case 756:
    case 757:
    case 758:
    case 759:
      return TYPE_ALTIVEC;

    case 577:
    case 606:
    case 607:
    case 678:
    case 699:
    case 704:
    case 705:
    case 706:
    case 707:
    case 708:
    case 709:
    case 710:
    case 711:
    case 712:
    case 713:
    case 714:
      return TYPE_VECFLOAT;

    case 624:
    case 625:
    case 626:
    case 627:
    case 628:
    case 629:
    case 647:
    case 648:
    case 649:
    case 650:
    case 651:
    case 652:
    case 653:
    case 654:
    case 655:
    case 656:
    case 657:
    case 665:
    case 666:
    case 673:
    case 674:
    case 692:
    case 693:
    case 694:
    case 700:
    case 701:
    case 702:
    case 703:
    case 715:
    case 716:
    case 717:
    case 718:
    case 719:
    case 720:
    case 721:
    case 722:
    case 723:
    case 724:
    case 725:
    case 726:
    case 727:
    case 728:
      return TYPE_VECPERM;

    case 593:
    case 597:
    case 598:
    case 605:
    case 620:
    case 636:
    case 729:
    case 730:
    case 731:
    case 732:
      return TYPE_VECCMP;

    case 608:
    case 609:
    case 610:
    case 611:
    case 612:
    case 613:
    case 621:
    case 622:
    case 623:
    case 637:
    case 638:
    case 639:
    case 640:
    case 641:
    case 642:
    case 643:
    case 644:
    case 686:
    case 687:
    case 688:
    case 689:
    case 690:
      return TYPE_VECCOMPLEX;

    case 570:
    case 571:
    case 572:
    case 573:
    case 574:
    case 575:
    case 576:
    case 578:
    case 579:
    case 580:
    case 581:
    case 582:
    case 583:
    case 584:
    case 585:
    case 586:
    case 587:
    case 588:
    case 589:
    case 590:
    case 591:
    case 592:
    case 594:
    case 595:
    case 596:
    case 599:
    case 600:
    case 601:
    case 602:
    case 603:
    case 604:
    case 614:
    case 615:
    case 616:
    case 617:
    case 618:
    case 619:
    case 630:
    case 631:
    case 632:
    case 633:
    case 634:
    case 635:
    case 645:
    case 646:
    case 658:
    case 659:
    case 660:
    case 661:
    case 662:
    case 663:
    case 664:
    case 667:
    case 668:
    case 669:
    case 670:
    case 671:
    case 672:
    case 675:
    case 676:
    case 677:
    case 679:
    case 680:
    case 681:
    case 682:
    case 683:
    case 684:
    case 685:
    case 691:
    case 695:
    case 696:
    case 697:
    case 698:
    case 733:
    case 734:
    case 735:
    case 736:
    case 737:
    case 738:
    case 739:
    case 740:
      return TYPE_VECSIMPLE;

    case 381:
    case 383:
    case 385:
    case 387:
    case 389:
    case 391:
    case 520:
    case 521:
    case 522:
    case 523:
    case 524:
    case 549:
    case 550:
      return TYPE_JMPREG;

    case 189:
    case 205:
      return TYPE_DSQRT;

    case 188:
      return TYPE_SSQRT;

    case 175:
    case 198:
      return TYPE_DDIV;

    case 174:
      return TYPE_SDIV;

    case 173:
    case 177:
    case 179:
    case 182:
    case 183:
    case 186:
    case 197:
    case 199:
    case 200:
    case 201:
    case 202:
    case 203:
    case 204:
      return TYPE_DMUL;

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
    case 176:
    case 178:
    case 180:
    case 181:
    case 184:
    case 185:
    case 187:
    case 190:
    case 191:
    case 192:
    case 193:
    case 194:
    case 195:
    case 196:
    case 206:
    case 207:
    case 211:
    case 212:
    case 215:
    case 312:
    case 313:
    case 314:
    case 320:
    case 321:
    case 322:
      return TYPE_FP;

    case 410:
    case 411:
    case 412:
      return TYPE_FPCOMPARE;

    case 49:
    case 50:
    case 61:
    case 62:
    case 63:
    case 64:
    case 118:
    case 119:
    case 121:
    case 122:
    case 124:
    case 125:
    case 127:
    case 128:
    case 131:
    case 132:
    case 133:
    case 134:
    case 136:
    case 137:
    case 140:
    case 141:
    case 142:
    case 143:
    case 145:
    case 146:
    case 148:
    case 149:
    case 151:
    case 152:
    case 158:
    case 159:
    case 160:
    case 161:
    case 256:
    case 257:
    case 259:
    case 260:
    case 262:
    case 263:
    case 265:
    case 266:
    case 268:
    case 269:
    case 271:
    case 272:
    case 274:
    case 275:
    case 277:
    case 278:
    case 280:
    case 281:
    case 283:
    case 284:
    case 415:
    case 417:
    case 453:
    case 483:
    case 484:
    case 486:
      return TYPE_DELAYED_COMPARE;

    case 413:
    case 414:
    case 416:
    case 515:
    case 516:
    case 517:
    case 541:
    case 546:
    case 547:
    case 1072:
    case 1073:
      return TYPE_CR_LOGICAL;

    case 1:
    case 2:
    case 4:
    case 5:
    case 7:
    case 8:
    case 10:
    case 11:
    case 13:
    case 14:
    case 16:
    case 17:
    case 19:
    case 20:
    case 22:
    case 23:
    case 25:
    case 26:
    case 28:
    case 29:
    case 31:
    case 32:
    case 34:
    case 35:
    case 37:
    case 38:
    case 40:
    case 41:
    case 44:
    case 45:
    case 46:
    case 47:
    case 56:
    case 57:
    case 71:
    case 72:
    case 89:
    case 90:
    case 92:
    case 93:
    case 95:
    case 96:
    case 101:
    case 102:
    case 103:
    case 104:
    case 112:
    case 113:
    case 233:
    case 234:
    case 236:
    case 237:
    case 239:
    case 240:
    case 244:
    case 245:
    case 251:
    case 252:
    case 289:
    case 290:
    case 292:
    case 293:
    case 295:
    case 296:
    case 301:
    case 325:
    case 404:
    case 405:
    case 406:
    case 407:
    case 420:
    case 421:
    case 423:
    case 424:
    case 430:
    case 431:
    case 432:
    case 433:
    case 437:
    case 438:
    case 442:
    case 443:
    case 444:
    case 446:
    case 447:
    case 450:
    case 451:
    case 455:
    case 456:
    case 459:
    case 461:
    case 462:
    case 465:
    case 467:
    case 468:
    case 472:
    case 473:
    case 475:
    case 476:
    case 479:
    case 480:
    case 489:
    case 490:
    case 491:
    case 492:
    case 494:
    case 495:
    case 501:
    case 502:
    case 505:
    case 506:
    case 507:
    case 508:
      return TYPE_COMPARE;

    case 371:
    case 372:
    case 373:
    case 375:
    case 377:
    case 378:
    case 379:
    case 380:
    case 382:
    case 384:
    case 386:
    case 388:
    case 390:
    case 392:
    case 393:
    case 394:
    case 395:
    case 396:
    case 397:
    case 398:
    case 399:
    case 400:
    case 401:
    case 402:
    case 511:
    case 512:
    case 513:
    case 514:
    case 519:
    case 526:
    case 527:
    case 528:
    case 529:
    case 530:
    case 531:
    case 532:
    case 533:
    case 534:
    case 535:
    case 536:
    case 537:
      return TYPE_BRANCH;

    case 253:
    case 254:
      return TYPE_LDIV;

    case 65:
    case 66:
    case 67:
    case 68:
    case 69:
    case 73:
    case 76:
    case 77:
    case 78:
    case 79:
      return TYPE_IDIV;

    case 247:
    case 248:
    case 249:
      return TYPE_LMUL;

    case 74:
    case 75:
    case 220:
    case 221:
    case 222:
    case 223:
    case 224:
    case 225:
    case 226:
    case 227:
      return TYPE_IMUL;

    case 560:
    case 561:
    case 562:
    case 563:
    case 748:
    case 749:
    case 750:
    case 751:
    case 752:
      return TYPE_VECSTORE;

    case 556:
    case 557:
    case 558:
    case 559:
    case 741:
    case 742:
    case 743:
    case 744:
    case 745:
    case 746:
    case 747:
      return TYPE_VECLOAD;

    case 364:
    case 368:
      return TYPE_FPSTORE;

    case 363:
    case 367:
      return TYPE_FPLOAD;

    case 335:
    case 336:
    case 353:
    case 355:
    case 359:
    case 362:
    case 366:
      return TYPE_STORE;

    case 299:
    case 329:
    case 330:
    case 331:
    case 332:
    case 333:
    case 334:
    case 337:
    case 338:
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
    case 349:
    case 350:
    case 351:
    case 352:
    case 354:
    case 356:
    case 357:
    case 358:
    case 360:
    case 361:
    case 365:
    case 369:
    case 370:
    case 374:
    case 555:
      return TYPE_LOAD;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return TYPE_INTEGER;

    }
}

static int fpu2_unit_blockage PARAMS ((rtx, rtx));
static int
fpu2_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 412:
    case 411:
    case 410:
      if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 10 /* 0xa */;
        }
      else
        {
	  casenum = 12 /* 0xc */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case 205:
    case 189:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 12 /* 0xc */;
        }
      break;

    case 188:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 12 /* 0xc */;
        }
      break;

    case 198:
    case 175:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 174:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 322:
    case 321:
    case 320:
    case 314:
    case 313:
    case 312:
    case 215:
    case 212:
    case 211:
    case 207:
    case 206:
    case 204:
    case 203:
    case 202:
    case 201:
    case 200:
    case 199:
    case 197:
    case 196:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 187:
    case 186:
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
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 12 /* 0xc */;
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
      return 1;

    case 3:
      return 17 /* 0x11 */;

    case 4:
      return 17 /* 0x11 */;

    case 5:
      return 21 /* 0x15 */;

    case 6:
      return 26 /* 0x1a */;

    case 7:
      return 18 /* 0x12 */;

    case 8:
      return 26 /* 0x1a */;

    case 9:
      return 1;

    case 10:
      return 1;

    case 11:
      return 28 /* 0x1c */;

    case 12:
      return 35 /* 0x23 */;

    default:
      abort ();
    }
}

static int fpu2_unit_conflict_cost PARAMS ((rtx, rtx));
static int
fpu2_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 412:
    case 411:
    case 410:
      if ((((rs6000_cpu_attr) == (CPU_RIOS2))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 10 /* 0xa */;
        }
      else
        {
	  casenum = 12 /* 0xc */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case 205:
    case 189:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 12 /* 0xc */;
        }
      break;

    case 188:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 12 /* 0xc */;
        }
      break;

    case 198:
    case 175:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 174:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 322:
    case 321:
    case 320:
    case 314:
    case 313:
    case 312:
    case 215:
    case 212:
    case 211:
    case 207:
    case 206:
    case 204:
    case 203:
    case 202:
    case 201:
    case 200:
    case 199:
    case 197:
    case 196:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 187:
    case 186:
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
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 2;
        }
      else
        {
	  casenum = 9;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 12 /* 0xc */;
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
      return 1;

    case 3:
      return 17 /* 0x11 */;

    case 4:
      return 17 /* 0x11 */;

    case 5:
      return 21 /* 0x15 */;

    case 6:
      return 26 /* 0x1a */;

    case 7:
      return 18 /* 0x12 */;

    case 8:
      return 26 /* 0x1a */;

    case 9:
      return 1;

    case 10:
      return 1;

    case 11:
      return 28 /* 0x1c */;

    case 12:
      return 35 /* 0x23 */;

    default:
      abort ();
    }
}

static int vec_alu2_unit_blockage PARAMS ((rtx, rtx));
static int
vec_alu2_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
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
    case 703:
    case 702:
    case 701:
    case 700:
    case 694:
    case 693:
    case 692:
    case 674:
    case 673:
    case 666:
    case 665:
    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 651:
    case 650:
    case 649:
    case 648:
    case 647:
    case 629:
    case 628:
    case 627:
    case 626:
    case 625:
    case 624:
      casenum = 8;
      break;

    case 690:
    case 689:
    case 688:
    case 687:
    case 686:
    case 644:
    case 643:
    case 642:
    case 641:
    case 640:
    case 639:
    case 638:
    case 637:
    case 623:
    case 622:
    case 621:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
      casenum = 2;
      break;

    case 732:
    case 731:
    case 730:
    case 729:
    case 636:
    case 620:
    case 605:
    case 598:
    case 597:
    case 593:
      casenum = 4;
      break;

    case 714:
    case 713:
    case 712:
    case 711:
    case 710:
    case 709:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 699:
    case 678:
    case 607:
    case 606:
    case 577:
      casenum = 6;
      break;

    case 740:
    case 739:
    case 738:
    case 737:
    case 736:
    case 735:
    case 734:
    case 733:
    case 698:
    case 697:
    case 696:
    case 695:
    case 691:
    case 685:
    case 684:
    case 683:
    case 682:
    case 681:
    case 680:
    case 679:
    case 677:
    case 676:
    case 675:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 659:
    case 658:
    case 646:
    case 645:
    case 635:
    case 634:
    case 633:
    case 632:
    case 631:
    case 630:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 596:
    case 595:
    case 594:
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
    case 576:
    case 575:
    case 574:
    case 573:
    case 572:
    case 571:
    case 570:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 9;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      switch (recog_memoized (insn))
	{
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
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
        case 694:
        case 693:
        case 692:
        case 690:
        case 689:
        case 688:
        case 687:
        case 686:
        case 678:
        case 674:
        case 673:
        case 666:
        case 665:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
        case 636:
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
        case 613:
        case 612:
        case 611:
        case 610:
        case 609:
        case 608:
        case 607:
        case 606:
        case 605:
        case 598:
        case 597:
        case 593:
        case 577:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 1:
      switch (recog_memoized (insn))
	{
        case 740:
        case 739:
        case 738:
        case 737:
        case 736:
        case 735:
        case 734:
        case 733:
        case 698:
        case 697:
        case 696:
        case 695:
        case 691:
        case 685:
        case 684:
        case 683:
        case 682:
        case 681:
        case 680:
        case 679:
        case 677:
        case 676:
        case 675:
        case 672:
        case 671:
        case 670:
        case 669:
        case 668:
        case 667:
        case 664:
        case 663:
        case 662:
        case 661:
        case 660:
        case 659:
        case 658:
        case 646:
        case 645:
        case 635:
        case 634:
        case 633:
        case 632:
        case 631:
        case 630:
        case 619:
        case 618:
        case 617:
        case 616:
        case 615:
        case 614:
        case 604:
        case 603:
        case 602:
        case 601:
        case 600:
        case 599:
        case 596:
        case 595:
        case 594:
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
        case 576:
        case 575:
        case 574:
        case 573:
        case 572:
        case 571:
        case 570:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 2:
      switch (recog_memoized (insn))
	{
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
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
        case 691:
        case 685:
        case 684:
        case 683:
        case 682:
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
        case 659:
        case 658:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 646:
        case 645:
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
        case 620:
        case 619:
        case 618:
        case 617:
        case 616:
        case 615:
        case 614:
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
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 3:
      switch (recog_memoized (insn))
	{
        case 690:
        case 689:
        case 688:
        case 687:
        case 686:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
        case 623:
        case 622:
        case 621:
        case 613:
        case 612:
        case 611:
        case 610:
        case 609:
        case 608:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 4:
      switch (recog_memoized (insn))
	{
        case 740:
        case 739:
        case 738:
        case 737:
        case 736:
        case 735:
        case 734:
        case 733:
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
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
        case 659:
        case 658:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 646:
        case 645:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
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
        case 624:
        case 623:
        case 622:
        case 621:
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
        case 596:
        case 595:
        case 594:
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
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 5:
      switch (recog_memoized (insn))
	{
        case 732:
        case 731:
        case 730:
        case 729:
        case 636:
        case 620:
        case 605:
        case 598:
        case 597:
        case 593:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 6:
      switch (recog_memoized (insn))
	{
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
        case 703:
        case 702:
        case 701:
        case 700:
        case 698:
        case 697:
        case 696:
        case 695:
        case 694:
        case 693:
        case 692:
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
        case 681:
        case 680:
        case 679:
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
        case 659:
        case 658:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 646:
        case 645:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
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
        case 586:
        case 585:
        case 584:
        case 583:
        case 582:
        case 581:
        case 580:
        case 579:
        case 578:
        case 576:
        case 575:
        case 574:
        case 573:
        case 572:
        case 571:
        case 570:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 7:
      switch (recog_memoized (insn))
	{
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
        case 708:
        case 707:
        case 706:
        case 705:
        case 704:
        case 699:
        case 678:
        case 607:
        case 606:
        case 577:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 8:
      switch (recog_memoized (insn))
	{
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
        case 708:
        case 707:
        case 706:
        case 705:
        case 704:
        case 699:
        case 698:
        case 697:
        case 696:
        case 695:
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
        case 681:
        case 680:
        case 679:
        case 678:
        case 677:
        case 676:
        case 675:
        case 672:
        case 671:
        case 670:
        case 669:
        case 668:
        case 667:
        case 664:
        case 663:
        case 662:
        case 661:
        case 660:
        case 659:
        case 658:
        case 646:
        case 645:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
        case 636:
        case 635:
        case 634:
        case 633:
        case 632:
        case 631:
        case 630:
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
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 9:
      switch (recog_memoized (insn))
	{
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
        case 703:
        case 702:
        case 701:
        case 700:
        case 694:
        case 693:
        case 692:
        case 674:
        case 673:
        case 666:
        case 665:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 629:
        case 628:
        case 627:
        case 626:
        case 625:
        case 624:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    default:
      abort ();
    }
}

static int vec_alu2_unit_conflict_cost PARAMS ((rtx, rtx));
static int
vec_alu2_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
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
    case 703:
    case 702:
    case 701:
    case 700:
    case 694:
    case 693:
    case 692:
    case 674:
    case 673:
    case 666:
    case 665:
    case 657:
    case 656:
    case 655:
    case 654:
    case 653:
    case 652:
    case 651:
    case 650:
    case 649:
    case 648:
    case 647:
    case 629:
    case 628:
    case 627:
    case 626:
    case 625:
    case 624:
      casenum = 8;
      break;

    case 690:
    case 689:
    case 688:
    case 687:
    case 686:
    case 644:
    case 643:
    case 642:
    case 641:
    case 640:
    case 639:
    case 638:
    case 637:
    case 623:
    case 622:
    case 621:
    case 613:
    case 612:
    case 611:
    case 610:
    case 609:
    case 608:
      casenum = 2;
      break;

    case 732:
    case 731:
    case 730:
    case 729:
    case 636:
    case 620:
    case 605:
    case 598:
    case 597:
    case 593:
      casenum = 4;
      break;

    case 714:
    case 713:
    case 712:
    case 711:
    case 710:
    case 709:
    case 708:
    case 707:
    case 706:
    case 705:
    case 704:
    case 699:
    case 678:
    case 607:
    case 606:
    case 577:
      casenum = 6;
      break;

    case 740:
    case 739:
    case 738:
    case 737:
    case 736:
    case 735:
    case 734:
    case 733:
    case 698:
    case 697:
    case 696:
    case 695:
    case 691:
    case 685:
    case 684:
    case 683:
    case 682:
    case 681:
    case 680:
    case 679:
    case 677:
    case 676:
    case 675:
    case 672:
    case 671:
    case 670:
    case 669:
    case 668:
    case 667:
    case 664:
    case 663:
    case 662:
    case 661:
    case 660:
    case 659:
    case 658:
    case 646:
    case 645:
    case 635:
    case 634:
    case 633:
    case 632:
    case 631:
    case 630:
    case 619:
    case 618:
    case 617:
    case 616:
    case 615:
    case 614:
    case 604:
    case 603:
    case 602:
    case 601:
    case 600:
    case 599:
    case 596:
    case 595:
    case 594:
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
    case 576:
    case 575:
    case 574:
    case 573:
    case 572:
    case 571:
    case 570:
      casenum = 0;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 9;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      switch (recog_memoized (insn))
	{
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
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
        case 694:
        case 693:
        case 692:
        case 690:
        case 689:
        case 688:
        case 687:
        case 686:
        case 678:
        case 674:
        case 673:
        case 666:
        case 665:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
        case 636:
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
        case 613:
        case 612:
        case 611:
        case 610:
        case 609:
        case 608:
        case 607:
        case 606:
        case 605:
        case 598:
        case 597:
        case 593:
        case 577:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 1:
      switch (recog_memoized (insn))
	{
        case 740:
        case 739:
        case 738:
        case 737:
        case 736:
        case 735:
        case 734:
        case 733:
        case 698:
        case 697:
        case 696:
        case 695:
        case 691:
        case 685:
        case 684:
        case 683:
        case 682:
        case 681:
        case 680:
        case 679:
        case 677:
        case 676:
        case 675:
        case 672:
        case 671:
        case 670:
        case 669:
        case 668:
        case 667:
        case 664:
        case 663:
        case 662:
        case 661:
        case 660:
        case 659:
        case 658:
        case 646:
        case 645:
        case 635:
        case 634:
        case 633:
        case 632:
        case 631:
        case 630:
        case 619:
        case 618:
        case 617:
        case 616:
        case 615:
        case 614:
        case 604:
        case 603:
        case 602:
        case 601:
        case 600:
        case 599:
        case 596:
        case 595:
        case 594:
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
        case 576:
        case 575:
        case 574:
        case 573:
        case 572:
        case 571:
        case 570:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 2:
      switch (recog_memoized (insn))
	{
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
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
        case 691:
        case 685:
        case 684:
        case 683:
        case 682:
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
        case 659:
        case 658:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 646:
        case 645:
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
        case 620:
        case 619:
        case 618:
        case 617:
        case 616:
        case 615:
        case 614:
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
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 3:
      switch (recog_memoized (insn))
	{
        case 690:
        case 689:
        case 688:
        case 687:
        case 686:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
        case 623:
        case 622:
        case 621:
        case 613:
        case 612:
        case 611:
        case 610:
        case 609:
        case 608:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 4:
      switch (recog_memoized (insn))
	{
        case 740:
        case 739:
        case 738:
        case 737:
        case 736:
        case 735:
        case 734:
        case 733:
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
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
        case 659:
        case 658:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 646:
        case 645:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
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
        case 624:
        case 623:
        case 622:
        case 621:
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
        case 596:
        case 595:
        case 594:
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
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 5:
      switch (recog_memoized (insn))
	{
        case 732:
        case 731:
        case 730:
        case 729:
        case 636:
        case 620:
        case 605:
        case 598:
        case 597:
        case 593:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 6:
      switch (recog_memoized (insn))
	{
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
        case 703:
        case 702:
        case 701:
        case 700:
        case 698:
        case 697:
        case 696:
        case 695:
        case 694:
        case 693:
        case 692:
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
        case 681:
        case 680:
        case 679:
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
        case 659:
        case 658:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 646:
        case 645:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
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
        case 586:
        case 585:
        case 584:
        case 583:
        case 582:
        case 581:
        case 580:
        case 579:
        case 578:
        case 576:
        case 575:
        case 574:
        case 573:
        case 572:
        case 571:
        case 570:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 7:
      switch (recog_memoized (insn))
	{
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
        case 708:
        case 707:
        case 706:
        case 705:
        case 704:
        case 699:
        case 678:
        case 607:
        case 606:
        case 577:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    case 8:
      switch (recog_memoized (insn))
	{
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
        case 714:
        case 713:
        case 712:
        case 711:
        case 710:
        case 709:
        case 708:
        case 707:
        case 706:
        case 705:
        case 704:
        case 699:
        case 698:
        case 697:
        case 696:
        case 695:
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
        case 681:
        case 680:
        case 679:
        case 678:
        case 677:
        case 676:
        case 675:
        case 672:
        case 671:
        case 670:
        case 669:
        case 668:
        case 667:
        case 664:
        case 663:
        case 662:
        case 661:
        case 660:
        case 659:
        case 658:
        case 646:
        case 645:
        case 644:
        case 643:
        case 642:
        case 641:
        case 640:
        case 639:
        case 638:
        case 637:
        case 636:
        case 635:
        case 634:
        case 633:
        case 632:
        case 631:
        case 630:
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
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 2;

      }

    case 9:
      switch (recog_memoized (insn))
	{
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
        case 703:
        case 702:
        case 701:
        case 700:
        case 694:
        case 693:
        case 692:
        case 674:
        case 673:
        case 666:
        case 665:
        case 657:
        case 656:
        case 655:
        case 654:
        case 653:
        case 652:
        case 651:
        case 650:
        case 649:
        case 648:
        case 647:
        case 629:
        case 628:
        case 627:
        case 626:
        case 625:
        case 624:
	  return 0;

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 1;

      }

    default:
      abort ();
    }
}

static int imuldiv_unit_blockage PARAMS ((rtx, rtx));
static int
imuldiv_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 11) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 309:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 6) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 254:
    case 253:
      if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 249:
    case 248:
    case 247:
      if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 2;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC604)))
        {
	  casenum = 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC604E)))
        {
	  casenum = 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 14 /* 0xe */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 18 /* 0x12 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_PPC604E))))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC620)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 23 /* 0x17 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 60:
    case 59:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  casenum = 2;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC604))))
        {
	  casenum = 4;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC604E))))
        {
	  casenum = 5;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))
        {
	  casenum = 6;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))
        {
	  casenum = 7;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))
        {
	  casenum = 8;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 15 /* 0xf */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 20 /* 0x14 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 24 /* 0x18 */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 2;

    case 1:
      return 13 /* 0xd */;

    case 2:
      return 1;

    case 3:
      return 6;

    case 4:
      return 2;

    case 5:
      return 1;

    case 6:
      return 3;

    case 7:
      return 3;

    case 8:
      return 3;

    case 9:
      return 5;

    case 10:
      return 19 /* 0x13 */;

    case 11:
      return 36 /* 0x24 */;

    case 12:
      return 20 /* 0x14 */;

    case 13:
      return 36 /* 0x24 */;

    case 14:
      return 2;

    case 15:
      return 1;

    case 16:
      return 23 /* 0x17 */;

    case 17:
      return 1;

    case 18:
      return 4;

    case 19:
      return 2;

    case 20:
      return 1;

    case 21:
      return 19 /* 0x13 */;

    case 22:
      return 2;

    case 23:
      return 35 /* 0x23 */;

    case 24:
      return 67 /* 0x43 */;

    default:
      abort ();
    }
}

static int imuldiv_unit_conflict_cost PARAMS ((rtx, rtx));
static int
imuldiv_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 11) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 309:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 6) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 254:
    case 253:
      if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 249:
    case 248:
    case 247:
      if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 2;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC604)))
        {
	  casenum = 4;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC604E)))
        {
	  casenum = 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 14 /* 0xe */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 18 /* 0x12 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || (((rs6000_cpu_attr) == (CPU_PPC604E))))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC620)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC630)))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 23 /* 0x17 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case 60:
    case 59:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  casenum = 2;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC604))))
        {
	  casenum = 4;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC604E))))
        {
	  casenum = 5;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))
        {
	  casenum = 6;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))
        {
	  casenum = 7;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))
        {
	  casenum = 8;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 15 /* 0xf */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 20 /* 0x14 */;
        }
      else
        {
	  casenum = 24 /* 0x18 */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 24 /* 0x18 */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 2;

    case 1:
      return 13 /* 0xd */;

    case 2:
      return 1;

    case 3:
      return 6;

    case 4:
      return 2;

    case 5:
      return 1;

    case 6:
      return 3;

    case 7:
      return 3;

    case 8:
      return 3;

    case 9:
      return 5;

    case 10:
      return 19 /* 0x13 */;

    case 11:
      return 36 /* 0x24 */;

    case 12:
      return 20 /* 0x14 */;

    case 13:
      return 36 /* 0x24 */;

    case 14:
      return 2;

    case 15:
      return 1;

    case 16:
      return 23 /* 0x17 */;

    case 17:
      return 1;

    case 18:
      return 4;

    case 19:
      return 2;

    case 20:
      return 1;

    case 21:
      return 19 /* 0x13 */;

    case 22:
      return 2;

    case 23:
      return 35 /* 0x23 */;

    case 24:
      return 67 /* 0x43 */;

    default:
      abort ();
    }
}

static int iu2_unit_blockage PARAMS ((rtx, rtx));
static int
iu2_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 435:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 328:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 309:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 308:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 305:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 555:
    case 374:
    case 370:
    case 369:
    case 367:
    case 365:
    case 363:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 254:
    case 253:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 249:
    case 248:
    case 247:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 14 /* 0xe */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 87:
    case 86:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 7;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 15 /* 0xf */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 60:
    case 59:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 3;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 7;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 8;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 9;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 16 /* 0x10 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 486:
    case 484:
    case 483:
    case 453:
    case 417:
    case 415:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 161:
    case 160:
    case 159:
    case 158:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 143:
    case 142:
    case 141:
    case 140:
    case 137:
    case 136:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 64:
    case 63:
    case 62:
    case 61:
    case 50:
    case 49:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 508:
    case 507:
    case 506:
    case 505:
    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 480:
    case 479:
    case 476:
    case 475:
    case 473:
    case 472:
    case 468:
    case 467:
    case 465:
    case 462:
    case 461:
    case 459:
    case 456:
    case 455:
    case 451:
    case 450:
    case 447:
    case 446:
    case 444:
    case 443:
    case 442:
    case 438:
    case 437:
    case 433:
    case 432:
    case 431:
    case 430:
    case 424:
    case 423:
    case 421:
    case 420:
    case 407:
    case 406:
    case 405:
    case 404:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 252:
    case 251:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 234:
    case 233:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 72:
    case 71:
    case 57:
    case 56:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 38:
    case 37:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1016:
    case 1015:
    case 554:
    case 553:
    case 552:
    case 551:
    case 548:
    case 545:
    case 544:
    case 543:
    case 542:
    case 540:
    case 539:
    case 538:
    case 525:
    case 518:
    case 510:
    case 509:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 493:
    case 488:
    case 487:
    case 485:
    case 482:
    case 481:
    case 478:
    case 477:
    case 474:
    case 471:
    case 470:
    case 469:
    case 466:
    case 464:
    case 463:
    case 460:
    case 458:
    case 457:
    case 454:
    case 452:
    case 449:
    case 448:
    case 445:
    case 441:
    case 440:
    case 439:
    case 436:
    case 434:
    case 429:
    case 428:
    case 427:
    case 426:
    case 425:
    case 422:
    case 419:
    case 418:
    case 409:
    case 408:
    case 403:
    case 376:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 298:
    case 297:
    case 294:
    case 291:
    case 288:
    case 285:
    case 282:
    case 279:
    case 276:
    case 273:
    case 270:
    case 267:
    case 264:
    case 261:
    case 258:
    case 255:
    case 250:
    case 246:
    case 243:
    case 242:
    case 241:
    case 238:
    case 235:
    case 232:
    case 231:
    case 230:
    case 229:
    case 228:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 210:
    case 209:
    case 208:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 147:
    case 144:
    case 139:
    case 138:
    case 135:
    case 130:
    case 129:
    case 126:
    case 123:
    case 120:
    case 117:
    case 116:
    case 115:
    case 114:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 105:
    case 100:
    case 99:
    case 98:
    case 97:
    case 94:
    case 91:
    case 88:
    case 85:
    case 80:
    case 70:
    case 58:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 48:
    case 43:
    case 42:
    case 39:
    case 36:
    case 27:
    case 21:
    case 3:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))
        {
	  casenum = 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    default:
      casenum = 20 /* 0x14 */;
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
      return 1;

    case 3:
      return 2;

    case 4:
      return 13 /* 0xd */;

    case 5:
      return 1;

    case 6:
      return 1;

    case 7:
      return 4;

    case 8:
      return 2;

    case 9:
      return 1;

    case 10:
      return 19 /* 0x13 */;

    case 11:
      return 1;

    case 12:
      return 1;

    case 13:
      return 1;

    case 14:
      return 6;

    case 15:
      return 4;

    case 16:
      return 3;

    case 17:
      return 35 /* 0x23 */;

    case 18:
      return 67 /* 0x43 */;

    case 19:
      return 1;

    case 20:
      return 1;

    default:
      abort ();
    }
}

static int iu2_unit_conflict_cost PARAMS ((rtx, rtx));
static int
iu2_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 435:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 328:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 309:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 308:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 305:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 555:
    case 374:
    case 370:
    case 369:
    case 367:
    case 365:
    case 363:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 254:
    case 253:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 249:
    case 248:
    case 247:
      if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 14 /* 0xe */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 87:
    case 86:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 3;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 7;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 15 /* 0xf */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 4;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 60:
    case 59:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 3;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 7;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 8;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 9;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 16 /* 0x10 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 486:
    case 484:
    case 483:
    case 453:
    case 417:
    case 415:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 161:
    case 160:
    case 159:
    case 158:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 143:
    case 142:
    case 141:
    case 140:
    case 137:
    case 136:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 64:
    case 63:
    case 62:
    case 61:
    case 50:
    case 49:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 508:
    case 507:
    case 506:
    case 505:
    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 480:
    case 479:
    case 476:
    case 475:
    case 473:
    case 472:
    case 468:
    case 467:
    case 465:
    case 462:
    case 461:
    case 459:
    case 456:
    case 455:
    case 451:
    case 450:
    case 447:
    case 446:
    case 444:
    case 443:
    case 442:
    case 438:
    case 437:
    case 433:
    case 432:
    case 431:
    case 430:
    case 424:
    case 423:
    case 421:
    case 420:
    case 407:
    case 406:
    case 405:
    case 404:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 252:
    case 251:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 234:
    case 233:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 72:
    case 71:
    case 57:
    case 56:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 38:
    case 37:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS2))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_POWER4))))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1016:
    case 1015:
    case 554:
    case 553:
    case 552:
    case 551:
    case 548:
    case 545:
    case 544:
    case 543:
    case 542:
    case 540:
    case 539:
    case 538:
    case 525:
    case 518:
    case 510:
    case 509:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 493:
    case 488:
    case 487:
    case 485:
    case 482:
    case 481:
    case 478:
    case 477:
    case 474:
    case 471:
    case 470:
    case 469:
    case 466:
    case 464:
    case 463:
    case 460:
    case 458:
    case 457:
    case 454:
    case 452:
    case 449:
    case 448:
    case 445:
    case 441:
    case 440:
    case 439:
    case 436:
    case 434:
    case 429:
    case 428:
    case 427:
    case 426:
    case 425:
    case 422:
    case 419:
    case 418:
    case 409:
    case 408:
    case 403:
    case 376:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 298:
    case 297:
    case 294:
    case 291:
    case 288:
    case 285:
    case 282:
    case 279:
    case 276:
    case 273:
    case 270:
    case 267:
    case 264:
    case 261:
    case 258:
    case 255:
    case 250:
    case 246:
    case 243:
    case 242:
    case 241:
    case 238:
    case 235:
    case 232:
    case 231:
    case 230:
    case 229:
    case 228:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 210:
    case 209:
    case 208:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 147:
    case 144:
    case 139:
    case 138:
    case 135:
    case 130:
    case 129:
    case 126:
    case 123:
    case 120:
    case 117:
    case 116:
    case 115:
    case 114:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 105:
    case 100:
    case 99:
    case 98:
    case 97:
    case 94:
    case 91:
    case 88:
    case 85:
    case 80:
    case 70:
    case 58:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 48:
    case 43:
    case 42:
    case 39:
    case 36:
    case 27:
    case 21:
    case 3:
      if (((rs6000_cpu_attr) == (CPU_RIOS2)))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))
        {
	  casenum = 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_POWER4)))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 20 /* 0x14 */;
        }
      break;

    default:
      casenum = 20 /* 0x14 */;
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
      return 1;

    case 3:
      return 2;

    case 4:
      return 13 /* 0xd */;

    case 5:
      return 1;

    case 6:
      return 1;

    case 7:
      return 4;

    case 8:
      return 2;

    case 9:
      return 1;

    case 10:
      return 19 /* 0x13 */;

    case 11:
      return 1;

    case 12:
      return 1;

    case 13:
      return 1;

    case 14:
      return 6;

    case 15:
      return 4;

    case 16:
      return 3;

    case 17:
      return 35 /* 0x23 */;

    case 18:
      return 67 /* 0x43 */;

    case 19:
      return 1;

    case 20:
      return 1;

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
    case 412:
    case 411:
    case 410:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620))))))))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 368:
    case 364:
      casenum = 0;
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 7;
        }
      else if ((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 8;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 7;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 8;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 7;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 8;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 188:
      casenum = 25 /* 0x19 */;
      break;

    case 198:
    case 175:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 23 /* 0x17 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC603)))
        {
	  casenum = 24 /* 0x18 */;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 174:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620))))))
        {
	  casenum = 20 /* 0x14 */;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 204:
    case 203:
    case 202:
    case 201:
    case 200:
    case 199:
    case 197:
    case 186:
    case 183:
    case 182:
    case 179:
    case 177:
    case 173:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 9;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))
        {
	  casenum = 14 /* 0xe */;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 322:
    case 321:
    case 320:
    case 314:
    case 313:
    case 312:
    case 215:
    case 212:
    case 211:
    case 207:
    case 206:
    case 196:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 187:
    case 185:
    case 184:
    case 181:
    case 180:
    case 178:
    case 176:
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
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 7;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 8;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 26 /* 0x1a */;
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
      return 1;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 1;

    case 6:
      return 2;

    case 7:
      return 1;

    case 8:
      return 1;

    case 9:
      return 1;

    case 10:
      return 2;

    case 11:
      return 5;

    case 12:
      return 2;

    case 13:
      return 2;

    case 14:
      return 1;

    case 15:
      return 19 /* 0x13 */;

    case 16:
      return 31 /* 0x1f */;

    case 17:
      return 17 /* 0x11 */;

    case 18:
      return 21 /* 0x15 */;

    case 19:
      return 10 /* 0xa */;

    case 20:
      return 18 /* 0x12 */;

    case 21:
      return 17 /* 0x11 */;

    case 22:
      return 31 /* 0x1f */;

    case 23:
      return 35 /* 0x23 */;

    case 24:
      return 33 /* 0x21 */;

    case 25:
      return 31 /* 0x1f */;

    case 26:
      return 31 /* 0x1f */;

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
    case 412:
    case 411:
    case 410:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 1;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620))))))))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC7400))) || (((rs6000_cpu_attr) == (CPU_PPC7450)))))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 368:
    case 364:
      casenum = 0;
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 7;
        }
      else if ((which_alternative == 7) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 8;
        }
      else if ((which_alternative == 7) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 7;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 8;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE)))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 7;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 8;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 188:
      casenum = 25 /* 0x19 */;
      break;

    case 198:
    case 175:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))
        {
	  casenum = 22 /* 0x16 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 23 /* 0x17 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC603)))
        {
	  casenum = 24 /* 0x18 */;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 174:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || (((rs6000_cpu_attr) == (CPU_PPC620))))))
        {
	  casenum = 20 /* 0x14 */;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 204:
    case 203:
    case 202:
    case 201:
    case 200:
    case 199:
    case 197:
    case 186:
    case 183:
    case 182:
    case 179:
    case 177:
    case 173:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 5;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 9;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((rs6000_cpu_attr) == (CPU_MPCCORE)))
        {
	  casenum = 11 /* 0xb */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || (((rs6000_cpu_attr) == (CPU_PPC750))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))
        {
	  casenum = 14 /* 0xe */;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case 322:
    case 321:
    case 320:
    case 314:
    case 313:
    case 312:
    case 215:
    case 212:
    case 211:
    case 207:
    case 206:
    case 196:
    case 195:
    case 194:
    case 193:
    case 192:
    case 191:
    case 190:
    case 187:
    case 185:
    case 184:
    case 181:
    case 180:
    case 178:
    case 176:
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
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 5;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || (((rs6000_cpu_attr) == (CPU_MPCCORE))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 7;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))
        {
	  casenum = 8;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 9;
        }
      else
        {
	  casenum = 26 /* 0x1a */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 26 /* 0x1a */;
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
      return 1;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 1;

    case 6:
      return 2;

    case 7:
      return 1;

    case 8:
      return 1;

    case 9:
      return 1;

    case 10:
      return 2;

    case 11:
      return 5;

    case 12:
      return 2;

    case 13:
      return 2;

    case 14:
      return 1;

    case 15:
      return 19 /* 0x13 */;

    case 16:
      return 31 /* 0x1f */;

    case 17:
      return 17 /* 0x11 */;

    case 18:
      return 21 /* 0x15 */;

    case 19:
      return 10 /* 0xa */;

    case 20:
      return 18 /* 0x12 */;

    case 21:
      return 17 /* 0x11 */;

    case 22:
      return 31 /* 0x1f */;

    case 23:
      return 35 /* 0x23 */;

    case 24:
      return 33 /* 0x21 */;

    case 25:
      return 31 /* 0x1f */;

    case 26:
      return 31 /* 0x1f */;

    default:
      abort ();
    }
}

static int iu_unit_blockage PARAMS ((rtx, rtx));
static int
iu_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 435:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if (((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 367:
    case 363:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 2;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 328:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 309:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 308:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else if (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 254:
    case 253:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 249:
    case 248:
    case 247:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 87:
    case 86:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_PPC403)))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC405)))
        {
	  casenum = 7;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 9;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC403)))
        {
	  casenum = 20 /* 0x14 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC405)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 23 /* 0x17 */;
        }
      break;

    case 60:
    case 59:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC403))))
        {
	  casenum = 6;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_PPC405))))
        {
	  casenum = 7;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_PPC405))))
        {
	  casenum = 8;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 9;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 10 /* 0xa */;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  casenum = 15 /* 0xf */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 486:
    case 484:
    case 483:
    case 453:
    case 417:
    case 415:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 161:
    case 160:
    case 159:
    case 158:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 143:
    case 142:
    case 141:
    case 140:
    case 137:
    case 136:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 64:
    case 63:
    case 62:
    case 61:
    case 50:
    case 49:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 508:
    case 507:
    case 506:
    case 505:
    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 480:
    case 479:
    case 476:
    case 475:
    case 473:
    case 472:
    case 468:
    case 467:
    case 465:
    case 462:
    case 461:
    case 459:
    case 456:
    case 455:
    case 451:
    case 450:
    case 447:
    case 446:
    case 444:
    case 443:
    case 442:
    case 438:
    case 437:
    case 433:
    case 432:
    case 431:
    case 430:
    case 424:
    case 423:
    case 421:
    case 420:
    case 407:
    case 406:
    case 405:
    case 404:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 252:
    case 251:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 234:
    case 233:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 72:
    case 71:
    case 57:
    case 56:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 38:
    case 37:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1016:
    case 1015:
    case 554:
    case 553:
    case 552:
    case 551:
    case 548:
    case 545:
    case 544:
    case 543:
    case 542:
    case 540:
    case 539:
    case 538:
    case 525:
    case 518:
    case 510:
    case 509:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 493:
    case 488:
    case 487:
    case 485:
    case 482:
    case 481:
    case 478:
    case 477:
    case 474:
    case 471:
    case 470:
    case 469:
    case 466:
    case 464:
    case 463:
    case 460:
    case 458:
    case 457:
    case 454:
    case 452:
    case 449:
    case 448:
    case 445:
    case 441:
    case 440:
    case 439:
    case 436:
    case 434:
    case 429:
    case 428:
    case 427:
    case 426:
    case 425:
    case 422:
    case 419:
    case 418:
    case 409:
    case 408:
    case 403:
    case 376:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 298:
    case 297:
    case 294:
    case 291:
    case 288:
    case 285:
    case 282:
    case 279:
    case 276:
    case 273:
    case 270:
    case 267:
    case 264:
    case 261:
    case 258:
    case 255:
    case 250:
    case 246:
    case 243:
    case 242:
    case 241:
    case 238:
    case 235:
    case 232:
    case 231:
    case 230:
    case 229:
    case 228:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 210:
    case 209:
    case 208:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 147:
    case 144:
    case 139:
    case 138:
    case 135:
    case 130:
    case 129:
    case 126:
    case 123:
    case 120:
    case 117:
    case 116:
    case 115:
    case 114:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 105:
    case 100:
    case 99:
    case 98:
    case 97:
    case 94:
    case 91:
    case 88:
    case 85:
    case 80:
    case 70:
    case 58:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 48:
    case 43:
    case 42:
    case 39:
    case 36:
    case 27:
    case 21:
    case 3:
      if ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    default:
      casenum = 27 /* 0x1b */;
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
      return 1;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 1;

    case 6:
      return 4;

    case 7:
      return 3;

    case 8:
      return 2;

    case 9:
      return 5;

    case 10:
      return 4;

    case 11:
      return 3;

    case 12:
      return 5;

    case 13:
      return 20 /* 0x14 */;

    case 14:
      return 12 /* 0xc */;

    case 15:
      return 8;

    case 16:
      return 34 /* 0x22 */;

    case 17:
      return 19 /* 0x13 */;

    case 18:
      return 66 /* 0x42 */;

    case 19:
      return 66 /* 0x42 */;

    case 20:
      return 33 /* 0x21 */;

    case 21:
      return 35 /* 0x23 */;

    case 22:
      return 36 /* 0x24 */;

    case 23:
      return 36 /* 0x24 */;

    case 24:
      return 1;

    case 25:
      return 1;

    case 26:
      return 1;

    case 27:
      return 2;

    default:
      abort ();
    }
}

static int iu_unit_conflict_cost PARAMS ((rtx, rtx));
static int
iu_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 435:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || (which_alternative == 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if (((which_alternative != 0) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((which_alternative == 0) || (which_alternative == 2)) || ((which_alternative != 0) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 1073:
    case 1072:
    case 547:
    case 546:
    case 541:
    case 517:
    case 516:
    case 515:
    case 416:
    case 414:
    case 413:
      if ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 367:
    case 363:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 2;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 3;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 368:
    case 366:
    case 364:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 328:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 10) || (which_alternative == 12))))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 3) && (which_alternative != 4)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 309:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 308:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 323:
    case 307:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC601))))
        {
	  casenum = 3;
        }
      else if (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 7)))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4))))) && ((which_alternative == 5) || (which_alternative == 6))) || (which_alternative == 8)) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else if (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))))) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 7))))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 1;
        }
      else if (((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 10))) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601))))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 287:
    case 286:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if (((which_alternative == 1) || (which_alternative == 4)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 254:
    case 253:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 249:
    case 248:
    case 247:
      if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 87:
    case 86:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if ((which_alternative == 0) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 84:
    case 83:
    case 82:
    case 81:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 75:
    case 74:
      if (((rs6000_cpu_attr) == (CPU_PPC403)))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC405)))
        {
	  casenum = 7;
        }
      else if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 9;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 13 /* 0xd */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 79:
    case 78:
    case 77:
    case 76:
    case 73:
    case 69:
    case 68:
    case 67:
    case 66:
    case 65:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_RS64A)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC403)))
        {
	  casenum = 20 /* 0x14 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC405)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC601)))
        {
	  casenum = 22 /* 0x16 */;
        }
      else
        {
	  casenum = 23 /* 0x17 */;
        }
      break;

    case 60:
    case 59:
      extract_insn_cached (insn);
      if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && (((rs6000_cpu_attr) == (CPU_PPC403))))
        {
	  casenum = 6;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_PPC405))))
        {
	  casenum = 7;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode))) && (((rs6000_cpu_attr) == (CPU_PPC405))))
        {
	  casenum = 8;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 9;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 10 /* 0xa */;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RIOS1))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if (((get_attr_type (insn) == TYPE_IMUL) || ((get_attr_type (insn) == TYPE_IMUL2) || (s8bit_cint_operand (operands[2], SImode)))) && ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((get_attr_type (insn) == TYPE_IMUL2) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if ((s8bit_cint_operand (operands[2], SImode)) && (((rs6000_cpu_attr) == (CPU_RS64A))))
        {
	  casenum = 15 /* 0xf */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 486:
    case 484:
    case 483:
    case 453:
    case 417:
    case 415:
    case 284:
    case 283:
    case 281:
    case 280:
    case 278:
    case 277:
    case 275:
    case 274:
    case 272:
    case 271:
    case 269:
    case 268:
    case 266:
    case 265:
    case 263:
    case 262:
    case 260:
    case 259:
    case 257:
    case 256:
    case 161:
    case 160:
    case 159:
    case 158:
    case 152:
    case 151:
    case 149:
    case 148:
    case 146:
    case 145:
    case 143:
    case 142:
    case 141:
    case 140:
    case 137:
    case 136:
    case 134:
    case 133:
    case 132:
    case 131:
    case 128:
    case 127:
    case 125:
    case 124:
    case 122:
    case 121:
    case 119:
    case 118:
    case 64:
    case 63:
    case 62:
    case 61:
    case 50:
    case 49:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 25 /* 0x19 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 508:
    case 507:
    case 506:
    case 505:
    case 502:
    case 501:
    case 495:
    case 494:
    case 492:
    case 491:
    case 490:
    case 489:
    case 480:
    case 479:
    case 476:
    case 475:
    case 473:
    case 472:
    case 468:
    case 467:
    case 465:
    case 462:
    case 461:
    case 459:
    case 456:
    case 455:
    case 451:
    case 450:
    case 447:
    case 446:
    case 444:
    case 443:
    case 442:
    case 438:
    case 437:
    case 433:
    case 432:
    case 431:
    case 430:
    case 424:
    case 423:
    case 421:
    case 420:
    case 407:
    case 406:
    case 405:
    case 404:
    case 325:
    case 301:
    case 296:
    case 295:
    case 293:
    case 292:
    case 290:
    case 289:
    case 252:
    case 251:
    case 245:
    case 244:
    case 240:
    case 239:
    case 237:
    case 236:
    case 234:
    case 233:
    case 113:
    case 112:
    case 104:
    case 103:
    case 102:
    case 101:
    case 96:
    case 95:
    case 93:
    case 92:
    case 90:
    case 89:
    case 72:
    case 71:
    case 57:
    case 56:
    case 47:
    case 46:
    case 45:
    case 44:
    case 41:
    case 40:
    case 38:
    case 37:
    case 35:
    case 34:
    case 32:
    case 31:
    case 29:
    case 28:
    case 26:
    case 25:
    case 23:
    case 22:
    case 20:
    case 19:
    case 17:
    case 16:
    case 14:
    case 13:
    case 11:
    case 10:
    case 8:
    case 7:
    case 5:
    case 4:
    case 2:
    case 1:
      if (((rs6000_cpu_attr) == (CPU_RIOS1)))
        {
	  casenum = 24 /* 0x18 */;
        }
      else if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))
        {
	  casenum = 26 /* 0x1a */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || (((rs6000_cpu_attr) == (CPU_PPC601)))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603))))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 1016:
    case 1015:
    case 554:
    case 553:
    case 552:
    case 551:
    case 548:
    case 545:
    case 544:
    case 543:
    case 542:
    case 540:
    case 539:
    case 538:
    case 525:
    case 518:
    case 510:
    case 509:
    case 504:
    case 503:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 493:
    case 488:
    case 487:
    case 485:
    case 482:
    case 481:
    case 478:
    case 477:
    case 474:
    case 471:
    case 470:
    case 469:
    case 466:
    case 464:
    case 463:
    case 460:
    case 458:
    case 457:
    case 454:
    case 452:
    case 449:
    case 448:
    case 445:
    case 441:
    case 440:
    case 439:
    case 436:
    case 434:
    case 429:
    case 428:
    case 427:
    case 426:
    case 425:
    case 422:
    case 419:
    case 418:
    case 409:
    case 408:
    case 403:
    case 376:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 298:
    case 297:
    case 294:
    case 291:
    case 288:
    case 285:
    case 282:
    case 279:
    case 276:
    case 273:
    case 270:
    case 267:
    case 264:
    case 261:
    case 258:
    case 255:
    case 250:
    case 246:
    case 243:
    case 242:
    case 241:
    case 238:
    case 235:
    case 232:
    case 231:
    case 230:
    case 229:
    case 228:
    case 219:
    case 218:
    case 217:
    case 216:
    case 214:
    case 213:
    case 210:
    case 209:
    case 208:
    case 157:
    case 156:
    case 155:
    case 154:
    case 153:
    case 150:
    case 147:
    case 144:
    case 139:
    case 138:
    case 135:
    case 130:
    case 129:
    case 126:
    case 123:
    case 120:
    case 117:
    case 116:
    case 115:
    case 114:
    case 111:
    case 110:
    case 109:
    case 108:
    case 107:
    case 106:
    case 105:
    case 100:
    case 99:
    case 98:
    case 97:
    case 94:
    case 91:
    case 88:
    case 85:
    case 80:
    case 70:
    case 58:
    case 55:
    case 54:
    case 53:
    case 52:
    case 51:
    case 48:
    case 43:
    case 42:
    case 39:
    case 36:
    case 27:
    case 21:
    case 3:
      if ((((rs6000_cpu_attr) == (CPU_RIOS1))) || ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC403))) || ((((rs6000_cpu_attr) == (CPU_PPC405))) || ((((rs6000_cpu_attr) == (CPU_PPC601))) || (((rs6000_cpu_attr) == (CPU_PPC603)))))))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    default:
      casenum = 27 /* 0x1b */;
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
      return 1;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 1;

    case 6:
      return 4;

    case 7:
      return 3;

    case 8:
      return 2;

    case 9:
      return 5;

    case 10:
      return 4;

    case 11:
      return 3;

    case 12:
      return 5;

    case 13:
      return 20 /* 0x14 */;

    case 14:
      return 12 /* 0xc */;

    case 15:
      return 8;

    case 16:
      return 34 /* 0x22 */;

    case 17:
      return 19 /* 0x13 */;

    case 18:
      return 66 /* 0x42 */;

    case 19:
      return 66 /* 0x42 */;

    case 20:
      return 33 /* 0x21 */;

    case 21:
      return 35 /* 0x23 */;

    case 22:
      return 36 /* 0x24 */;

    case 23:
      return 36 /* 0x24 */;

    case 24:
      return 1;

    case 25:
      return 1;

    case 26:
      return 1;

    case 27:
      return 2;

    default:
      abort ();
    }
}

static int lsu_unit_blockage PARAMS ((rtx, rtx));
static int
lsu_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 752:
    case 751:
    case 750:
    case 749:
    case 748:
    case 563:
    case 562:
    case 561:
    case 560:
      casenum = 4;
      break;

    case 747:
    case 746:
    case 745:
    case 744:
    case 743:
    case 742:
    case 741:
    case 559:
    case 558:
    case 557:
    case 556:
      casenum = 1;
      break;

    case 368:
    case 364:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 367:
    case 363:
      if ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 366:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 8) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 328:
    case 308:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 8;
        }
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
      return 1;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 3;

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

static int lsu_unit_conflict_cost PARAMS ((rtx, rtx));
static int
lsu_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 752:
    case 751:
    case 750:
    case 749:
    case 748:
    case 563:
    case 562:
    case 561:
    case 560:
      casenum = 4;
      break;

    case 747:
    case 746:
    case 745:
    case 744:
    case 743:
    case 742:
    case 741:
    case 559:
    case 558:
    case 557:
    case 556:
      casenum = 1;
      break;

    case 368:
    case 364:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 5;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 367:
    case 363:
      if ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))
        {
	  casenum = 6;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 366:
    case 362:
    case 359:
    case 355:
    case 353:
    case 336:
    case 335:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630)))))))))
        {
	  casenum = 2;
        }
      else if ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))
        {
	  casenum = 3;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 327:
    case 326:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if (((which_alternative == 0) || (which_alternative == 1)) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 324:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 2) || (which_alternative == 9)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 8) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 8) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 311:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 310:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 306:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 5) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 323:
    case 309:
    case 307:
    case 305:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if (((which_alternative == 2) || (which_alternative == 5)) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else if ((which_alternative == 5) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 5;
        }
      else if ((which_alternative == 4) && ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))
        {
	  casenum = 6;
        }
      else if ((which_alternative == 4) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 7;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 304:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 9) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 10) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 10) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 328:
    case 308:
    case 303:
    case 302:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 1) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 1) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 300:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400))))))))))))
        {
	  casenum = 0;
        }
      else if ((which_alternative == 2) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 1;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || (((rs6000_cpu_attr) == (CPU_PPC630))))))))))
        {
	  casenum = 2;
        }
      else if ((which_alternative == 3) && ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))
        {
	  casenum = 3;
        }
      else if ((which_alternative == 3) && (((rs6000_cpu_attr) == (CPU_PPC7450))))
        {
	  casenum = 4;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 555:
    case 374:
    case 370:
    case 369:
    case 365:
    case 361:
    case 360:
    case 358:
    case 357:
    case 356:
    case 354:
    case 352:
    case 351:
    case 350:
    case 349:
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
    case 334:
    case 333:
    case 332:
    case 331:
    case 330:
    case 329:
    case 299:
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 8;
        }
      break;

    case 33:
    case 30:
    case 24:
    case 18:
    case 15:
    case 12:
    case 9:
    case 6:
    case 0:
      extract_constrain_insn_cached (insn);
      if ((((rs6000_cpu_attr) == (CPU_RS64A))) || ((((rs6000_cpu_attr) == (CPU_MPCCORE))) || ((((rs6000_cpu_attr) == (CPU_PPC603))) || ((((rs6000_cpu_attr) == (CPU_PPC604))) || ((((rs6000_cpu_attr) == (CPU_PPC604E))) || ((((rs6000_cpu_attr) == (CPU_PPC620))) || ((((rs6000_cpu_attr) == (CPU_PPC630))) || ((((rs6000_cpu_attr) == (CPU_PPC750))) || (((rs6000_cpu_attr) == (CPU_PPC7400)))))))))))
        {
	  casenum = 0;
        }
      else if (((rs6000_cpu_attr) == (CPU_PPC7450)))
        {
	  casenum = 1;
        }
      else
        {
	  casenum = 8;
        }
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
      return 1;

    case 3:
      return 1;

    case 4:
      return 1;

    case 5:
      return 3;

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
  {"lsu", 1, 1, 0, 0, 3, lsu_unit_ready_cost, lsu_unit_conflict_cost, 3, lsu_unit_blockage_range, lsu_unit_blockage}, 
  {"iu", 2, 1, 0, 0, 66, iu_unit_ready_cost, iu_unit_conflict_cost, 66, iu_unit_blockage_range, iu_unit_blockage}, 
  {"fpu", 4, 1, 0, 0, 35, fpu_unit_ready_cost, fpu_unit_conflict_cost, 35, fpu_unit_blockage_range, fpu_unit_blockage}, 
  {"iu2", 8, 2, 0, 0, 67, iu2_unit_ready_cost, iu2_unit_conflict_cost, 67, iu2_unit_blockage_range, iu2_unit_blockage}, 
  {"imuldiv", 16, 1, 0, 0, 67, imuldiv_unit_ready_cost, imuldiv_unit_conflict_cost, 67, imuldiv_unit_blockage_range, imuldiv_unit_blockage}, 
  {"iu3", 32, 3, 0, 1, 1, iu3_unit_ready_cost, 0, 1, 0, 0}, 
  {"vec_alu2", 64, 2, 0, 0, 2, vec_alu2_unit_ready_cost, vec_alu2_unit_conflict_cost, 2, vec_alu2_unit_blockage_range, vec_alu2_unit_blockage}, 
  {"sru", 128, 1, 0, 2, 2, sru_unit_ready_cost, 0, 2, 0, 0}, 
  {"fpu2", 256, 2, 0, 0, 35, fpu2_unit_ready_cost, fpu2_unit_conflict_cost, 35, fpu2_unit_blockage_range, fpu2_unit_blockage}, 
  {"bpu", 512, 1, 0, 1, 1, bpu_unit_ready_cost, 0, 1, 0, 0}, 
  {"cru", 1024, 1, 0, 1, 1, cru_unit_ready_cost, 0, 1, 0, 0}, 
  {"lsu2", 2048, 2, 0, 1, 1, lsu2_unit_ready_cost, 0, 1, 0, 0}, 
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

int length_unit_log = 2;
