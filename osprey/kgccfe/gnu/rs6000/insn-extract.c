/* Generated automatically by the program `genextract'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "toplev.h"

static rtx junk ATTRIBUTE_UNUSED;
void
insn_extract (insn)
     rtx insn;
{
  rtx *ro = recog_data.operand;
  rtx **ro_loc = recog_data.operand_loc;
  rtx pat = PATTERN (insn);
  int i ATTRIBUTE_UNUSED;

  memset (ro, 0, sizeof (*ro) * MAX_RECOG_OPERANDS);
  memset (ro_loc, 0, sizeof (*ro_loc) * MAX_RECOG_OPERANDS);
  switch (INSN_CODE (insn))
    {
    case -1:
      fatal_insn_not_found (insn);

    case 1073:
    case 1072:
    case 1016:
    case 1015:
      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
          ro[i] = *(ro_loc[i] = &XVECEXP (pat, 0, i));
      break;

    case 759:  /* altivec_abss_v4si */
    case 758:  /* altivec_abss_v8hi */
    case 757:  /* altivec_abss_v16qi */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 750:  /* altivec_stvebx */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 1));
      break;

    case 752:  /* altivec_stvewx */
    case 751:  /* altivec_stvehx */
    case 749:  /* altivec_stvxl */
    case 748:  /* altivec_stvx */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 1));
      break;

    case 745:  /* altivec_lvewx */
    case 744:  /* altivec_lvehx */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      break;

    case 746:  /* altivec_lvxl */
    case 743:  /* altivec_lvebx */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      break;

    case 740:  /* altivec_dststt */
    case 739:  /* altivec_dstst */
    case 738:  /* altivec_dstt */
    case 737:  /* altivec_dst */
      ro[0] = *(ro_loc[0] = &XVECEXP (pat, 0, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (pat, 0, 1));
      ro[2] = *(ro_loc[2] = &XVECEXP (pat, 0, 2));
      break;

    case 736:  /* altivec_dss */
      ro[0] = *(ro_loc[0] = &XVECEXP (pat, 0, 0));
      break;

    case 733:  /* altivec_mtvscr */
      ro[0] = *(ro_loc[0] = &XVECEXP (XEXP (pat, 1), 0, 0));
      break;

    case 732:  /* altivec_predicate_v16qi */
    case 731:  /* altivec_predicate_v8hi */
    case 730:  /* altivec_predicate_v4sf */
    case 729:  /* altivec_predicate_v4si */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 2));
      break;

    case 629:  /* altivec_vmrglw */
    case 628:  /* altivec_vmrglh */
    case 627:  /* altivec_vmrglb */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 622:  /* altivec_vmhraddshs */
    case 621:  /* altivec_vmhaddshs */
    case 613:  /* altivec_vmsumshs */
    case 612:  /* altivec_vmsumuhs */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 2));
      break;

    case 722:  /* altivec_vsldoi_16qi */
    case 721:  /* altivec_vsldoi_8hi */
    case 720:  /* altivec_vsldoi_4sf */
    case 719:  /* altivec_vsldoi_4si */
    case 718:  /* altivec_vsel_16qi */
    case 717:  /* altivec_vsel_8hi */
    case 716:  /* altivec_vsel_4sf */
    case 715:  /* altivec_vsel_4si */
    case 703:  /* altivec_vperm_16qi */
    case 702:  /* altivec_vperm_8hi */
    case 701:  /* altivec_vperm_4sf */
    case 700:  /* altivec_vperm_4si */
    case 623:  /* altivec_vmladduhm */
    case 611:  /* altivec_vmsumshm */
    case 610:  /* altivec_vmsumuhm */
    case 609:  /* altivec_vmsummbm */
    case 608:  /* altivec_vmsumubm */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (pat, 1), 0, 1));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (pat, 1), 0, 2));
      break;

    case 586:  /* altivec_vandc */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 710:  /* altivec_vctsxs */
    case 709:  /* altivec_vctuxs */
    case 690:  /* altivec_vsumsws */
    case 689:  /* altivec_vsum2sws */
    case 688:  /* altivec_vsum4shs */
    case 687:  /* altivec_vsum4sbs */
    case 686:  /* altivec_vsum4ubs */
    case 685:  /* altivec_vsubsws */
    case 684:  /* altivec_vsubuws */
    case 683:  /* altivec_vsubshs */
    case 682:  /* altivec_vsubuhs */
    case 681:  /* altivec_vsubsbs */
    case 680:  /* altivec_vsububs */
    case 657:  /* altivec_vpkswus */
    case 656:  /* altivec_vpkuwus */
    case 655:  /* altivec_vpkshus */
    case 654:  /* altivec_vpkuhus */
    case 653:  /* altivec_vpkswss */
    case 652:  /* altivec_vpkuwss */
    case 651:  /* altivec_vpkshss */
    case 650:  /* altivec_vpkuhss */
    case 584:  /* altivec_vaddsws */
    case 583:  /* altivec_vadduws */
    case 582:  /* altivec_vaddshs */
    case 581:  /* altivec_vadduhs */
    case 580:  /* altivec_vaddsbs */
    case 579:  /* altivec_vaddubs */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      break;

    case 569:  /* *set_vrsave_internal */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      break;

    case 555:  /* prefetch */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      ro[2] = *(ro_loc[2] = &XEXP (pat, 2));
      break;

    case 554:  /* eh_set_lr_di */
    case 553:  /* eh_set_lr_si */
      ro[0] = *(ro_loc[0] = &XVECEXP (XVECEXP (pat, 0, 0), 0, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 552:  /* *return_and_restore_fpregs_di */
    case 551:  /* *return_and_restore_fpregs_si */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 1));
      break;

    case 546:  /* *movsi_to_cr */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      break;

    case 545:  /* stack_tie */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      recog_data.dup_loc[0] = &XVECEXP (XEXP (pat, 1), 0, 0);
      recog_data.dup_num[0] = 0;
      break;

    case 544:  /* *save_fpregs_di */
    case 543:  /* *save_fpregs_si */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 1));
      break;

    case 548:  /* *lmw */
    case 542:  /* *stmw */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 1));
      break;

    case 540:  /* *rs6000.md:14717 */
    case 539:  /* *rs6000.md:14709 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 0), 1));
      break;

    case 537:  /* *ctrdi_internal6 */
    case 535:  /* *ctrsi_internal6 */
    case 533:  /* *ctrdi_internal4 */
    case 531:  /* *ctrsi_internal4 */
    case 529:  /* *ctrdi_internal2 */
    case 527:  /* *ctrsi_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 2), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 536:  /* *ctrdi_internal5 */
    case 534:  /* *ctrsi_internal5 */
    case 532:  /* *ctrdi_internal3 */
    case 530:  /* *ctrsi_internal3 */
    case 528:  /* *ctrdi_internal1 */
    case 526:  /* *ctrsi_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 524:  /* *rs6000.md:14210 */
    case 523:  /* *rs6000.md:14200 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 0), 0));
      break;

    case 522:  /* indirect_jumpdi */
    case 521:  /* indirect_jumpsi */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 1));
      break;

    case 519:  /* jump */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 518:  /* *rs6000.md:14064 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 517:  /* *rs6000.md:14049 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 516:  /* *rs6000.md:14031 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0));
      break;

    case 515:  /* *rs6000.md:14011 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0));
      break;

    case 513:  /* *rs6000.md:13972 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 2), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 514:  /* *rs6000.md:13987 */
    case 512:  /* *rs6000.md:13956 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 511:  /* *rs6000.md:13941 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 492:  /* *rs6000.md:13553 */
    case 491:  /* *rs6000.md:13520 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 490:  /* *rs6000.md:13488 */
    case 489:  /* *rs6000.md:13456 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 626:  /* altivec_vmrghw */
    case 625:  /* altivec_vmrghh */
    case 624:  /* altivec_vmrghb */
    case 488:  /* *rs6000.md:13447 */
    case 487:  /* *rs6000.md:13438 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 480:  /* *rs6000.md:13284 */
    case 451:  /* *rs6000.md:12645 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 1);
      recog_data.dup_num[1] = 2;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[2] = 3;
      break;

    case 479:  /* *rs6000.md:13248 */
    case 450:  /* *rs6000.md:12611 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 478:  /* *rs6000.md:13236 */
    case 449:  /* *rs6000.md:12601 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 433:  /* *rs6000.md:12248 */
    case 432:  /* *rs6000.md:12207 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 431:  /* *rs6000.md:12172 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 430:  /* *rs6000.md:12134 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 429:  /* *rs6000.md:12123 */
    case 428:  /* *rs6000.md:12112 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 427:  /* *rs6000.md:12102 */
    case 426:  /* *ne0 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 747:  /* altivec_lvx */
    case 645:  /* altivec_vnor */
    case 510:  /* *rs6000.md:13929 */
    case 509:  /* *rs6000.md:13921 */
    case 498:  /* *rs6000.md:13675 */
    case 477:  /* *rs6000.md:13226 */
    case 469:  /* *rs6000.md:13052 */
    case 463:  /* *rs6000.md:12925 */
    case 457:  /* *rs6000.md:12794 */
    case 448:  /* *rs6000.md:12593 */
    case 439:  /* *rs6000.md:12416 */
    case 425:  /* *rs6000.md:12078 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 465:  /* *rs6000.md:12944 */
    case 435:  /* *rs6000.md:12300 */
    case 421:  /* *rs6000.md:11921 */
    case 420:  /* *rs6000.md:11879 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 417:  /* *rs6000.md:11751 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[1] = 2;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[2] = 3;
      break;

    case 416:  /* *rs6000.md:11725 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 415:  /* *rs6000.md:11694 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 1);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[1] = 2;
      break;

    case 414:  /* *rs6000.md:11684 */
    case 413:  /* *rs6000.md:11666 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 409:  /* *rs6000.md:11602 */
    case 408:  /* *rs6000.md:11592 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 735:  /* altivec_dssall */
    case 538:  /* trap */
    case 525:  /* nop */
    case 520:  /* return */
    case 403:  /* blockage */
      break;

    case 387:  /* *call_value_indirect_nonlocal_aix64 */
    case 385:  /* *call_value_indirect_nonlocal_aix32 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 383:  /* *call_indirect_nonlocal_aix64 */
    case 381:  /* *call_indirect_nonlocal_aix32 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 402:  /* *sibcall_value_nonlocal_sysv */
    case 400:  /* *sibcall_value_nonlocal_aix64 */
    case 399:  /* *sibcall_value_nonlocal_aix32 */
    case 396:  /* *sibcall_value_local64 */
    case 395:  /* *sibcall_value_local32 */
    case 392:  /* *call_value_nonlocal_sysv */
    case 391:  /* *call_value_indirect_nonlocal_sysv */
    case 388:  /* *call_value_nonlocal_aix64 */
    case 386:  /* *call_value_nonlocal_aix32 */
    case 380:  /* *call_value_local64 */
    case 379:  /* *call_value_local32 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 401:  /* *sibcall_nonlocal_sysv */
    case 398:  /* *sibcall_nonlocal_aix64 */
    case 397:  /* *sibcall_nonlocal_aix32 */
    case 394:  /* *sibcall_local64 */
    case 393:  /* *sibcall_local32 */
    case 390:  /* *call_nonlocal_sysv */
    case 389:  /* *call_indirect_nonlocal_sysv */
    case 384:  /* *call_nonlocal_aix64 */
    case 382:  /* *call_nonlocal_aix32 */
    case 378:  /* *call_local64 */
    case 377:  /* *call_local32 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 376:  /* macho_correct_pic */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XEXP (pat, 1), 1), 0, 0));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (XEXP (pat, 1), 1), 0, 1));
      break;

    case 374:  /* load_toc_v4_PIC_2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 1));
      break;

    case 373:  /* load_toc_v4_PIC_1b */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[2] = *(ro_loc[2] = &XVECEXP (XVECEXP (pat, 0, 1), 0, 1));
      recog_data.dup_loc[0] = &XVECEXP (XVECEXP (pat, 0, 1), 0, 0);
      recog_data.dup_num[0] = 1;
      break;

    case 372:  /* load_toc_v4_PIC_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      recog_data.dup_loc[0] = &XVECEXP (XVECEXP (pat, 0, 1), 0, 0);
      recog_data.dup_num[0] = 1;
      break;

    case 734:  /* altivec_mfvscr */
    case 568:  /* get_vrsave_internal */
    case 541:  /* movesi_from_cr */
    case 371:  /* load_toc_v4_pic_si */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      break;

    case 370:  /* load_toc_aix_di */
    case 369:  /* load_toc_aix_si */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      break;

    case 368:  /* *movdf_update2 */
    case 366:  /* *movsf_update4 */
    case 364:  /* *movsf_update2 */
    case 362:  /* *movqi_update3 */
    case 359:  /* *movhi_update4 */
    case 355:  /* movsi_update */
    case 353:  /* movdi_update */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 361:  /* *movqi_update2 */
    case 358:  /* *movhi_update3 */
    case 357:  /* *movhi_update2 */
    case 352:  /* *movdi_update2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 350:  /* *rs6000.md:9980 */
    case 349:  /* *rs6000.md:9967 */
    case 348:  /* *rs6000.md:9954 */
    case 347:  /* *rs6000.md:9930 */
    case 346:  /* *rs6000.md:9917 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 345:  /* *rs6000.md:9887 */
    case 344:  /* *rs6000.md:9868 */
    case 343:  /* *rs6000.md:9849 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 7), 0));
      break;

    case 342:  /* *rs6000.md:9812 */
    case 341:  /* *rs6000.md:9791 */
    case 340:  /* *rs6000.md:9770 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 9), 0));
      break;

    case 339:  /* *rs6000.md:9728 */
    case 338:  /* *rs6000.md:9704 */
    case 337:  /* *rs6000.md:9680 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 11), 0));
      break;

    case 336:  /* *store_multiple_string */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 335:  /* *store_multiple_power */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 334:  /* *ldmsi3 */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[1] = 1;
      break;

    case 333:  /* *ldmsi4 */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 3), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 3), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0), 0);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[2] = 1;
      break;

    case 332:  /* *ldmsi5 */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 4), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 4), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 3), 1), 0), 0);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0), 0);
      recog_data.dup_num[2] = 1;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[3] = 1;
      break;

    case 331:  /* *ldmsi6 */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 4), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 5), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 5), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 4), 1), 0), 0);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 3), 1), 0), 0);
      recog_data.dup_num[2] = 1;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0), 0);
      recog_data.dup_num[3] = 1;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[4] = 1;
      break;

    case 330:  /* *ldmsi7 */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 4), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 5), 0));
      ro[8] = *(ro_loc[8] = &XEXP (XVECEXP (pat, 0, 6), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 6), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 5), 1), 0), 0);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 4), 1), 0), 0);
      recog_data.dup_num[2] = 1;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 3), 1), 0), 0);
      recog_data.dup_num[3] = 1;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0), 0);
      recog_data.dup_num[4] = 1;
      recog_data.dup_loc[5] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[5] = 1;
      break;

    case 329:  /* *ldmsi8 */
      ro[0] = *(ro_loc[0] = &PATTERN (insn));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 4), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 5), 0));
      ro[8] = *(ro_loc[8] = &XEXP (XVECEXP (pat, 0, 6), 0));
      ro[9] = *(ro_loc[9] = &XEXP (XVECEXP (pat, 0, 7), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 7), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 6), 1), 0), 0);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 5), 1), 0), 0);
      recog_data.dup_num[2] = 1;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 4), 1), 0), 0);
      recog_data.dup_num[3] = 1;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 3), 1), 0), 0);
      recog_data.dup_num[4] = 1;
      recog_data.dup_loc[5] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0), 0);
      recog_data.dup_num[5] = 1;
      recog_data.dup_loc[6] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[6] = 1;
      break;

    case 326:  /* *movti_power */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 325:  /* *movdi_internal2 */
    case 301:  /* *movsi_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 1);
      recog_data.dup_num[0] = 1;
      break;

    case 573:  /* *movv16qi_const0 */
    case 572:  /* *movv8hi_const0 */
    case 571:  /* *movv4sf_const0 */
    case 570:  /* *movv4si_const0 */
    case 567:  /* *movv4sf_internal1 */
    case 566:  /* *movv16qi_internal1 */
    case 565:  /* *movv8hi_internal1 */
    case 564:  /* *movv4si_internal */
    case 563:  /* altivec_stvx_4sf */
    case 562:  /* altivec_stvx_16qi */
    case 561:  /* altivec_stvx_8hi */
    case 560:  /* altivec_stvx_4si */
    case 559:  /* altivec_lvx_4sf */
    case 558:  /* altivec_lvx_16qi */
    case 557:  /* altivec_lvx_8hi */
    case 556:  /* altivec_lvx_4si */
    case 328:  /* *movti_ppc64 */
    case 327:  /* *movti_string */
    case 324:  /* *movdi_internal64 */
    case 323:  /* *movdi_internal32 */
    case 311:  /* *movtf_internal */
    case 310:  /* *movdf_softfloat64 */
    case 309:  /* *movdf_hardfloat64 */
    case 308:  /* *movdf_softfloat32 */
    case 307:  /* *movdf_hardfloat32 */
    case 306:  /* *movsf_softfloat */
    case 305:  /* *movsf_hardfloat */
    case 304:  /* *movcc_internal1 */
    case 303:  /* *movqi_internal */
    case 302:  /* *movhi_internal */
    case 300:  /* *movsi_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      break;

    case 742:  /* altivec_lvsr */
    case 741:  /* altivec_lvsl */
    case 708:  /* altivec_vcfsx */
    case 707:  /* altivec_vcfux */
    case 694:  /* altivec_vspltw */
    case 693:  /* altivec_vsplth */
    case 692:  /* altivec_vspltb */
    case 679:  /* altivec_vsubcuw */
    case 674:  /* altivec_vsro */
    case 673:  /* altivec_vsr */
    case 672:  /* altivec_vsraw */
    case 671:  /* altivec_vsrah */
    case 670:  /* altivec_vsrab */
    case 669:  /* altivec_vsrw */
    case 668:  /* altivec_vsrh */
    case 667:  /* altivec_vsrb */
    case 666:  /* altivec_vslo */
    case 665:  /* altivec_vsl */
    case 664:  /* altivec_vslw_v4sf */
    case 663:  /* altivec_vslw */
    case 662:  /* altivec_vslh */
    case 661:  /* altivec_vslb */
    case 660:  /* altivec_vrlw */
    case 659:  /* altivec_vrlh */
    case 658:  /* altivec_vrlb */
    case 649:  /* altivec_vpkpx */
    case 648:  /* altivec_vpkuwum */
    case 647:  /* altivec_vpkuhum */
    case 644:  /* altivec_vmulosh */
    case 643:  /* altivec_vmulouh */
    case 642:  /* altivec_vmulosb */
    case 641:  /* altivec_vmuloub */
    case 640:  /* altivec_vmulesh */
    case 639:  /* altivec_vmuleuh */
    case 638:  /* altivec_vmulesb */
    case 637:  /* altivec_vmuleub */
    case 605:  /* altivec_vcmpgtfp */
    case 604:  /* altivec_vcmpgtsw */
    case 603:  /* altivec_vcmpgtuw */
    case 602:  /* altivec_vcmpgtsh */
    case 601:  /* altivec_vcmpgtuh */
    case 600:  /* altivec_vcmpgtsb */
    case 599:  /* altivec_vcmpgtub */
    case 598:  /* altivec_vcmpgefp */
    case 597:  /* altivec_vcmpeqfp */
    case 596:  /* altivec_vcmpequw */
    case 595:  /* altivec_vcmpequh */
    case 594:  /* altivec_vcmpequb */
    case 593:  /* altivec_vcmpbfp */
    case 592:  /* altivec_vavgsw */
    case 591:  /* altivec_vavguw */
    case 590:  /* altivec_vavgsh */
    case 589:  /* altivec_vavguh */
    case 588:  /* altivec_vavgsb */
    case 587:  /* altivec_vavgub */
    case 578:  /* altivec_vaddcuw */
    case 547:  /* *rs6000.md:14817 */
    case 299:  /* *movsi_got_internal */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (pat, 1), 0, 1));
      break;

    case 249:  /* umuldi3_highpart */
    case 248:  /* smuldi3_highpart */
    case 227:  /* *umulsi3_highpart_no_mq */
    case 225:  /* *smulsi3_highpart_no_mq */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1), 0));
      break;

    case 226:  /* umulsi3_highpart_mq */
    case 224:  /* smulsi3_highpart_mq */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 223:  /* *umulsidi3_no_mq */
    case 221:  /* *mulsidi3_no_mq */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 222:  /* umulsidi3_mq */
    case 220:  /* mulsidi3_mq */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 214:  /* floatunssidf_ppc64 */
    case 213:  /* floatsidf_ppc64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 211:  /* fctiwz */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 0));
      break;

    case 756:  /* absv4sf2 */
    case 755:  /* absv4si2 */
    case 754:  /* absv8hi2 */
    case 753:  /* absv16qi2 */
    case 210:  /* *fix_truncdfsi2_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 209:  /* *floatunssidf2_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 208:  /* *floatsidf2_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 4), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 5), 0));
      break;

    case 207:  /* *fselsfdf4 */
    case 206:  /* *fseldfdf4 */
    case 191:  /* *fseldfsf4 */
    case 190:  /* *fselsfsf4 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 2));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 204:  /* *rs6000.md:5594 */
    case 187:  /* *rs6000.md:5346 */
    case 185:  /* *rs6000.md:5327 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 202:  /* *rs6000.md:5574 */
    case 183:  /* *rs6000.md:5307 */
    case 181:  /* *rs6000.md:5288 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 203:  /* *rs6000.md:5584 */
    case 201:  /* *rs6000.md:5564 */
    case 186:  /* *rs6000.md:5337 */
    case 184:  /* *rs6000.md:5317 */
    case 182:  /* *rs6000.md:5298 */
    case 180:  /* *rs6000.md:5278 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 728:  /* altivec_vupklsh */
    case 727:  /* altivec_vupklpx */
    case 726:  /* altivec_vupklsb */
    case 725:  /* altivec_vupkhsh */
    case 724:  /* altivec_vupkhpx */
    case 723:  /* altivec_vupkhsb */
    case 714:  /* altivec_vrefp */
    case 713:  /* altivec_vrsqrtefp */
    case 712:  /* altivec_vexptefp */
    case 711:  /* altivec_vlogefp */
    case 706:  /* altivec_vrfim */
    case 705:  /* altivec_vrfin */
    case 704:  /* altivec_vrfip */
    case 698:  /* altivec_vspltisw_v4sf */
    case 697:  /* altivec_vspltisw */
    case 696:  /* altivec_vspltish */
    case 695:  /* altivec_vspltisb */
    case 375:  /* load_macho_picbase */
    case 164:  /* aux_truncdfsf2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      break;

    case 155:  /* *rs6000.md:4911 */
    case 154:  /* *rs6000.md:4902 */
    case 153:  /* *rs6000.md:4893 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 0), 2));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 269:  /* *rotldi3_internal15 */
    case 266:  /* *rotldi3_internal12 */
    case 263:  /* *rotldi3_internal9 */
    case 152:  /* *rs6000.md:4858 */
    case 149:  /* *rs6000.md:4779 */
    case 128:  /* *rotlsi3_internal12 */
    case 125:  /* *rotlsi3_internal9 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 268:  /* *rotldi3_internal14 */
    case 265:  /* *rotldi3_internal11 */
    case 262:  /* *rotldi3_internal8 */
    case 151:  /* *rs6000.md:4823 */
    case 148:  /* *rs6000.md:4744 */
    case 127:  /* *rotlsi3_internal11 */
    case 124:  /* *rotlsi3_internal8 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 267:  /* *rotldi3_internal13 */
    case 264:  /* *rotldi3_internal10 */
    case 261:  /* *rotldi3_internal7 */
    case 150:  /* *rs6000.md:4814 */
    case 147:  /* *rs6000.md:4735 */
    case 126:  /* *rotlsi3_internal10 */
    case 123:  /* *rotlsi3_internal7 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      break;

    case 508:  /* *rs6000.md:13886 */
    case 507:  /* *rs6000.md:13851 */
    case 495:  /* *rs6000.md:13626 */
    case 476:  /* *rs6000.md:13191 */
    case 468:  /* *rs6000.md:13019 */
    case 462:  /* *rs6000.md:12890 */
    case 456:  /* *rs6000.md:12761 */
    case 447:  /* *rs6000.md:12560 */
    case 438:  /* *rs6000.md:12381 */
    case 424:  /* *rs6000.md:12035 */
    case 278:  /* *ashldi3_internal9 */
    case 275:  /* *ashldi3_internal6 */
    case 260:  /* *rotldi3_internal6 */
    case 146:  /* *rs6000.md:4702 */
    case 137:  /* *rs6000.md:4463 */
    case 122:  /* *rotlsi3_internal6 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 2;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[2] = 3;
      break;

    case 506:  /* *rs6000.md:13817 */
    case 505:  /* *rs6000.md:13783 */
    case 494:  /* *rs6000.md:13595 */
    case 475:  /* *rs6000.md:13157 */
    case 467:  /* *rs6000.md:12987 */
    case 461:  /* *rs6000.md:12856 */
    case 455:  /* *rs6000.md:12729 */
    case 446:  /* *rs6000.md:12528 */
    case 437:  /* *rs6000.md:12347 */
    case 423:  /* *rs6000.md:11992 */
    case 277:  /* ashldi3_internal8 */
    case 274:  /* ashldi3_internal5 */
    case 259:  /* *rotldi3_internal5 */
    case 145:  /* *rs6000.md:4670 */
    case 136:  /* *rs6000.md:4431 */
    case 121:  /* *rotlsi3_internal5 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 607:  /* altivec_vnmsubfp */
    case 606:  /* altivec_vmaddfp */
    case 504:  /* *rs6000.md:13772 */
    case 503:  /* *rs6000.md:13761 */
    case 493:  /* *rs6000.md:13586 */
    case 474:  /* *rs6000.md:13146 */
    case 466:  /* *rs6000.md:12978 */
    case 460:  /* *rs6000.md:12845 */
    case 454:  /* *rs6000.md:12720 */
    case 445:  /* *rs6000.md:12519 */
    case 436:  /* *rs6000.md:12336 */
    case 422:  /* *rs6000.md:11978 */
    case 276:  /* *ashldi3_internal7 */
    case 273:  /* *ashldi3_internal4 */
    case 258:  /* *rotldi3_internal4 */
    case 200:  /* *rs6000.md:5555 */
    case 199:  /* *rs6000.md:5546 */
    case 179:  /* *rs6000.md:5269 */
    case 178:  /* *rs6000.md:5260 */
    case 177:  /* *rs6000.md:5251 */
    case 176:  /* *rs6000.md:5242 */
    case 144:  /* *rs6000.md:4662 */
    case 135:  /* *rs6000.md:4423 */
    case 120:  /* *rotlsi3_internal4 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 116:  /* *extzvdi_internal2 */
    case 113:  /* *extzvsi_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 2));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 2);
      recog_data.dup_num[2] = 3;
      break;

    case 115:  /* *extzvdi_internal1 */
    case 112:  /* *extzvsi_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 2));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 114:  /* extzvdi */
    case 111:  /* extzvsi */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 2));
      break;

    case 109:  /* *insvsi_internal4 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 0), 2));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (pat, 1), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (pat, 1), 2));
      break;

    case 108:  /* *insvsi_internal3 */
    case 107:  /* *insvsi_internal2 */
    case 106:  /* *insvsi_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 0), 2));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 110:  /* insvdi */
    case 105:  /* insvsi */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 0), 2));
      ro[3] = *(ro_loc[3] = &XEXP (pat, 1));
      break;

    case 104:  /* *maskir_internal8 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 3;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 2;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 0), 0);
      recog_data.dup_num[2] = 2;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 1);
      recog_data.dup_num[3] = 1;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0), 0);
      recog_data.dup_num[4] = 2;
      break;

    case 103:  /* *maskir_internal7 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 2;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 3;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 0), 0);
      recog_data.dup_num[2] = 2;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 1);
      recog_data.dup_num[3] = 1;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0), 0);
      recog_data.dup_num[4] = 2;
      break;

    case 102:  /* *maskir_internal6 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 0);
      recog_data.dup_num[0] = 2;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 0);
      recog_data.dup_num[2] = 3;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 1);
      recog_data.dup_num[3] = 2;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 1);
      recog_data.dup_num[4] = 2;
      break;

    case 101:  /* *maskir_internal5 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 0);
      recog_data.dup_num[0] = 2;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 0);
      recog_data.dup_num[2] = 2;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1), 1);
      recog_data.dup_num[3] = 3;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0);
      recog_data.dup_num[4] = 2;
      break;

    case 100:  /* *maskir_internal4 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0);
      recog_data.dup_num[0] = 2;
      break;

    case 99:  /* *maskir_internal3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0);
      recog_data.dup_num[0] = 2;
      break;

    case 98:  /* *maskir_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 1), 1);
      recog_data.dup_num[0] = 2;
      break;

    case 97:  /* *maskir_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 1), 0);
      recog_data.dup_num[0] = 2;
      break;

    case 296:  /* *boolccdi3_internal3 */
    case 96:  /* *boolccsi3_internal3 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 1);
      recog_data.dup_num[0] = 4;
      break;

    case 295:  /* *boolccdi3_internal2 */
    case 95:  /* *boolccsi3_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      break;

    case 294:  /* *boolccdi3_internal1 */
    case 94:  /* *boolccsi3_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (pat, 1));
      break;

    case 293:  /* *boolcdi3_internal3 */
    case 93:  /* *boolcsi3_internal3 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 1);
      recog_data.dup_num[0] = 4;
      break;

    case 292:  /* *boolcdi3_internal2 */
    case 92:  /* *boolcsi3_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      break;

    case 291:  /* *boolcdi3_internal1 */
    case 91:  /* *boolcsi3_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (pat, 1));
      break;

    case 290:  /* *booldi3_internal3 */
    case 90:  /* *boolsi3_internal3 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 1);
      recog_data.dup_num[0] = 4;
      break;

    case 289:  /* *booldi3_internal2 */
    case 89:  /* *boolsi3_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      break;

    case 288:  /* *booldi3_internal1 */
    case 88:  /* *boolsi3_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (pat, 1));
      break;

    case 86:  /* *andsi3_internal7 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 79:  /* quous_call */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 77:  /* divus_call */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 76:  /* divss_call */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 550:  /* *return_internal_di */
    case 549:  /* *return_internal_si */
    case 78:  /* quoss_call */
    case 75:  /* mull_call */
    case 74:  /* mulh_call */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 73:  /* *rs6000.md:2752 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1), 0);
      recog_data.dup_num[1] = 4;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[2] = 3;
      break;

    case 65:  /* *rs6000.md:2561 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 287:  /* *anddi3_internal3 */
    case 160:  /* *rs6000.md:5011 */
    case 142:  /* *rs6000.md:4595 */
    case 133:  /* *rs6000.md:4360 */
    case 84:  /* *andsi3_internal5 */
    case 83:  /* *andsi3_internal4 */
    case 63:  /* *rs6000.md:2476 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 286:  /* *anddi3_internal2 */
    case 158:  /* *rs6000.md:4952 */
    case 140:  /* *rs6000.md:4532 */
    case 131:  /* *rs6000.md:4301 */
    case 82:  /* *andsi3_internal3 */
    case 81:  /* *andsi3_internal2 */
    case 61:  /* *rs6000.md:2419 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 464:  /* *rs6000.md:12935 */
    case 434:  /* *rs6000.md:12289 */
    case 419:  /* *rs6000.md:11865 */
    case 418:  /* *rs6000.md:11851 */
    case 285:  /* anddi3 */
    case 230:  /* ashrdi3_power */
    case 229:  /* lshrdi3_power */
    case 228:  /* ashldi3_power */
    case 156:  /* ashrsi3_power */
    case 138:  /* lshrsi3_power */
    case 129:  /* ashlsi3_power */
    case 80:  /* andsi3 */
    case 68:  /* divsi3_mq */
    case 66:  /* udivsi3_mq */
    case 59:  /* mulsi3_mq */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 497:  /* *rs6000.md:13667 */
    case 496:  /* *rs6000.md:13659 */
    case 322:  /* *rs6000.md:9027 */
    case 194:  /* *rs6000.md:5507 */
    case 167:  /* *rs6000.md:5143 */
    case 53:  /* *nabs_power */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 319:  /* fix_trunctfsi2 */
    case 318:  /* fix_trunctfdi2 */
    case 317:  /* floatsitf2 */
    case 316:  /* floatditf2 */
    case 315:  /* trunctfsf2 */
    case 241:  /* absdi2 */
    case 216:  /* floatdisf2_internal1 */
    case 52:  /* abssi2_nopower */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 50:  /* *rs6000.md:2193 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 2;
      recog_data.dup_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 2), 0);
      recog_data.dup_num[2] = 2;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 2), 1);
      recog_data.dup_num[3] = 1;
      recog_data.dup_loc[4] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 2), 0);
      recog_data.dup_num[4] = 2;
      recog_data.dup_loc[5] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 2), 1);
      recog_data.dup_num[5] = 1;
      break;

    case 49:  /* *rs6000.md:2158 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 2), 0);
      recog_data.dup_num[0] = 2;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 2), 1);
      recog_data.dup_num[1] = 1;
      break;

    case 48:  /* *rs6000.md:2149 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 2), 0);
      recog_data.dup_num[0] = 2;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 2), 1);
      recog_data.dup_num[1] = 1;
      break;

    case 502:  /* *rs6000.md:13730 */
    case 501:  /* *rs6000.md:13699 */
    case 486:  /* *rs6000.md:13407 */
    case 473:  /* *rs6000.md:13113 */
    case 472:  /* *rs6000.md:13080 */
    case 459:  /* *rs6000.md:12812 */
    case 453:  /* *rs6000.md:12689 */
    case 444:  /* *rs6000.md:12504 */
    case 443:  /* *rs6000.md:12473 */
    case 442:  /* *rs6000.md:12442 */
    case 367:  /* *movdf_update1 */
    case 365:  /* *movsf_update3 */
    case 363:  /* *movsf_update1 */
    case 360:  /* *movqi_update1 */
    case 356:  /* *movhi_update */
    case 354:  /* *movsi_update1 */
    case 351:  /* *movdi_update1 */
    case 284:  /* *ashrdi3_internal3 */
    case 281:  /* *lshrdi3_internal3 */
    case 272:  /* *ashldi3_internal3 */
    case 257:  /* *rotldi3_internal3 */
    case 252:  /* *rs6000.md:6841 */
    case 240:  /* *rs6000.md:6595 */
    case 234:  /* *adddi3_internal3 */
    case 161:  /* *rs6000.md:5045 */
    case 143:  /* *rs6000.md:4631 */
    case 134:  /* *rs6000.md:4394 */
    case 119:  /* *rotlsi3_internal3 */
    case 87:  /* *andsi3_internal8 */
    case 72:  /* *rs6000.md:2723 */
    case 64:  /* *rs6000.md:2508 */
    case 47:  /* *rs6000.md:1984 */
    case 46:  /* *rs6000.md:1970 */
    case 38:  /* *addsi3_internal3 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 283:  /* *ashrdi3_internal2 */
    case 280:  /* *lshrdi3_internal2 */
    case 271:  /* *ashldi3_internal2 */
    case 256:  /* *rotldi3_internal2 */
    case 251:  /* *rs6000.md:6814 */
    case 239:  /* *rs6000.md:6568 */
    case 233:  /* *adddi3_internal2 */
    case 159:  /* *rs6000.md:4984 */
    case 141:  /* *rs6000.md:4566 */
    case 132:  /* *rs6000.md:4333 */
    case 118:  /* *rotlsi3_internal2 */
    case 71:  /* *rs6000.md:2696 */
    case 62:  /* *rs6000.md:2449 */
    case 45:  /* *rs6000.md:1942 */
    case 44:  /* *rs6000.md:1929 */
    case 37:  /* *addsi3_internal2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 691:  /* xorv4si3 */
    case 678:  /* subv4sf3 */
    case 677:  /* subv4si3 */
    case 676:  /* subv8hi3 */
    case 675:  /* subv16qi3 */
    case 646:  /* iorv4si3 */
    case 636:  /* sminv4sf3 */
    case 635:  /* sminv4si3 */
    case 634:  /* uminv4si3 */
    case 633:  /* sminv8hi3 */
    case 632:  /* uminv8hi3 */
    case 631:  /* sminv16qi3 */
    case 630:  /* uminv16qi3 */
    case 620:  /* smaxv4sf3 */
    case 619:  /* smaxv4si3 */
    case 618:  /* umaxv4si3 */
    case 617:  /* smaxv8hi3 */
    case 616:  /* umaxv8hi3 */
    case 615:  /* smaxv16qi3 */
    case 614:  /* umaxv16qi3 */
    case 585:  /* andv4si3 */
    case 577:  /* addv4sf3 */
    case 576:  /* addv4si3 */
    case 575:  /* addv8hi3 */
    case 574:  /* addv16qi3 */
    case 500:  /* *rs6000.md:13691 */
    case 499:  /* *rs6000.md:13683 */
    case 485:  /* *rs6000.md:13399 */
    case 471:  /* *rs6000.md:13070 */
    case 470:  /* *rs6000.md:13060 */
    case 458:  /* *rs6000.md:12802 */
    case 452:  /* *rs6000.md:12681 */
    case 441:  /* *rs6000.md:12434 */
    case 440:  /* *rs6000.md:12426 */
    case 412:  /* *cmptf_internal1 */
    case 411:  /* *cmpdf_internal1 */
    case 410:  /* *cmpsf_internal1 */
    case 407:  /* *cmpdi_internal2 */
    case 406:  /* *cmpsi_internal2 */
    case 405:  /* *cmpdi_internal1 */
    case 404:  /* *cmpsi_internal1 */
    case 298:  /* elf_low */
    case 282:  /* *ashrdi3_internal1 */
    case 279:  /* *lshrdi3_internal1 */
    case 270:  /* *ashldi3_internal1 */
    case 255:  /* rotldi3 */
    case 254:  /* udivdi3 */
    case 253:  /* *rs6000.md:6870 */
    case 250:  /* *rs6000.md:6806 */
    case 247:  /* muldi3 */
    case 238:  /* *rs6000.md:6559 */
    case 232:  /* *adddi3_internal1 */
    case 231:  /* ashrdi3_no_power */
    case 218:  /* *subdi3_noppc64 */
    case 217:  /* *adddi3_noppc64 */
    case 198:  /* divdf3 */
    case 197:  /* muldf3 */
    case 196:  /* subdf3 */
    case 195:  /* adddf3 */
    case 175:  /* *rs6000.md:5234 */
    case 174:  /* *rs6000.md:5226 */
    case 173:  /* *rs6000.md:5211 */
    case 172:  /* *rs6000.md:5203 */
    case 171:  /* *rs6000.md:5188 */
    case 170:  /* *rs6000.md:5180 */
    case 169:  /* *rs6000.md:5165 */
    case 168:  /* *rs6000.md:5157 */
    case 157:  /* ashrsi3_no_power */
    case 139:  /* lshrsi3_no_power */
    case 130:  /* ashlsi3_no_power */
    case 117:  /* rotlsi3 */
    case 85:  /* *andsi3_internal6 */
    case 70:  /* *rs6000.md:2688 */
    case 69:  /* *divsi3_no_mq */
    case 67:  /* *udivsi3_no_mq */
    case 60:  /* mulsi3_no_mq */
    case 43:  /* *rs6000.md:1920 */
    case 42:  /* *rs6000.md:1913 */
    case 36:  /* *addsi3_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 484:  /* *rs6000.md:13368 */
    case 483:  /* *rs6000.md:13337 */
    case 245:  /* *rs6000.md:6700 */
    case 237:  /* *rs6000.md:6532 */
    case 57:  /* *rs6000.md:2340 */
    case 41:  /* *rs6000.md:1886 */
    case 35:  /* *rs6000.md:1694 */
    case 32:  /* *rs6000.md:1627 */
    case 29:  /* *rs6000.md:1532 */
    case 26:  /* *rs6000.md:1459 */
    case 23:  /* *rs6000.md:1366 */
    case 20:  /* *rs6000.md:1293 */
    case 17:  /* *rs6000.md:1226 */
    case 14:  /* *rs6000.md:1159 */
    case 11:  /* *rs6000.md:1092 */
    case 8:  /* *rs6000.md:1025 */
    case 5:  /* *rs6000.md:958 */
    case 2:  /* *rs6000.md:900 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 244:  /* *rs6000.md:6675 */
    case 242:  /* *nabsdi2 */
    case 236:  /* *rs6000.md:6507 */
    case 56:  /* *rs6000.md:2315 */
    case 54:  /* *nabs_nopower */
    case 40:  /* *rs6000.md:1861 */
    case 34:  /* *rs6000.md:1669 */
    case 31:  /* *rs6000.md:1602 */
    case 28:  /* *rs6000.md:1507 */
    case 25:  /* *rs6000.md:1434 */
    case 22:  /* *rs6000.md:1341 */
    case 19:  /* *rs6000.md:1268 */
    case 16:  /* *rs6000.md:1201 */
    case 13:  /* *rs6000.md:1134 */
    case 10:  /* *rs6000.md:1067 */
    case 7:  /* *rs6000.md:1000 */
    case 4:  /* *rs6000.md:933 */
    case 1:  /* *rs6000.md:875 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 699:  /* ftruncv4sf2 */
    case 482:  /* *rs6000.md:13329 */
    case 481:  /* *rs6000.md:13321 */
    case 321:  /* abstf2 */
    case 320:  /* negtf2 */
    case 314:  /* trunctfdf2 */
    case 313:  /* extendsftf2 */
    case 312:  /* extenddftf2 */
    case 297:  /* elf_high */
    case 246:  /* ffsdi2 */
    case 243:  /* *rs6000.md:6669 */
    case 235:  /* one_cmpldi2 */
    case 219:  /* *negdi2_noppc64 */
    case 215:  /* fix_truncdfdi2 */
    case 212:  /* floatdidf2 */
    case 205:  /* sqrtdf2 */
    case 193:  /* absdf2 */
    case 192:  /* negdf2 */
    case 189:  /* *rs6000.md:5369 */
    case 188:  /* *rs6000.md:5362 */
    case 166:  /* *abssf2 */
    case 165:  /* *negsf2 */
    case 163:  /* truncdfsf2 */
    case 162:  /* extendsfdf2 */
    case 58:  /* ffssi2 */
    case 55:  /* negsi2 */
    case 51:  /* *abssi2_power */
    case 39:  /* one_cmplsi2 */
    case 33:  /* *rs6000.md:1660 */
    case 30:  /* *rs6000.md:1593 */
    case 27:  /* extendqihi2_ppc */
    case 24:  /* *rs6000.md:1425 */
    case 21:  /* extendqisi2_ppc */
    case 18:  /* *rs6000.md:1259 */
    case 15:  /* *rs6000.md:1192 */
    case 12:  /* *rs6000.md:1125 */
    case 9:  /* *rs6000.md:1058 */
    case 6:  /* *rs6000.md:991 */
    case 3:  /* extendqidi2 */
    case 0:  /* *rs6000.md:866 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      break;

    default:
      abort ();
    }
}
