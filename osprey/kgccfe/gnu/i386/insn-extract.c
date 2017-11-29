/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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

    case 1024:  /* monitor */
      ro[0] = *(ro_loc[0] = &XVECEXP (pat, 0, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (pat, 0, 1));
      ro[2] = *(ro_loc[2] = &XVECEXP (pat, 0, 2));
      break;

    case 1023:  /* mwait */
      ro[0] = *(ro_loc[0] = &XVECEXP (pat, 0, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (pat, 0, 1));
      break;

    case 1001:  /* sse2_punpcklqdq */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 989:  /* sse2_lshrti3 */
    case 988:  /* sse2_ashlti3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 1), 0));
      break;

    case 951:  /* sse2_umulv2siv2di3 */
    case 950:  /* sse2_umulsidi3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0));
      break;

    case 929:  /* cvtpd2ps */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0));
      break;

    case 882:  /* *prefetch_sse_rex */
    case 881:  /* *prefetch_sse */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 2));
      break;

    case 878:  /* pmulhrwv4hi3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 1), 0));
      break;

    case 870:  /* pi2fw */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0), 0), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 869:  /* pfpnacc */
    case 868:  /* pfnacc */
    case 867:  /* pfacc */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 1), 0);
      recog_data.dup_num[1] = 2;
      break;

    case 866:  /* pf2iw */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      break;

    case 857:  /* subrv2sf3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 854:  /* *sse_prologue_save_insn */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 3), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0), 1));
      break;

    case 1022:  /* *lfence_insn */
    case 1021:  /* *mfence_insn */
    case 853:  /* *sfence_insn */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      recog_data.dup_loc[0] = &XVECEXP (XEXP (pat, 1), 0, 0);
      recog_data.dup_num[0] = 0;
      break;

    case 958:  /* sse2_pextrw */
    case 821:  /* mmx_pextrw */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0, 0));
      break;

    case 957:  /* sse2_pinsrw */
    case 820:  /* mmx_pinsrw */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 2));
      break;

    case 1030:  /* hsubv2df3 */
    case 1029:  /* hsubv4sf3 */
    case 1028:  /* haddv2df3 */
    case 1027:  /* haddv4sf3 */
    case 1026:  /* addsubv2df3 */
    case 1025:  /* addsubv4sf3 */
    case 961:  /* sse2_pshufhw */
    case 960:  /* sse2_pshuflw */
    case 959:  /* sse2_pshufd */
    case 956:  /* sse2_psadbw */
    case 877:  /* pfrsqit1v2sf3 */
    case 875:  /* pfrcpit2v2sf3 */
    case 874:  /* pfrcpit1v2sf3 */
    case 872:  /* pavgusb */
    case 822:  /* mmx_pshufw */
    case 819:  /* mmx_psadbw */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (pat, 1), 0, 1));
      break;

    case 955:  /* sse2_uavgv8hi3 */
    case 954:  /* sse2_uavgv16qi3 */
    case 818:  /* mmx_uavgv4hi3 */
    case 817:  /* mmx_uavgv8qi3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      break;

    case 816:  /* mmx_nanddi3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 1));
      break;

    case 952:  /* sse2_pmaddwd */
    case 811:  /* mmx_pmaddwd */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 1), 0), 0);
      recog_data.dup_num[1] = 2;
      break;

    case 949:  /* umulv8hi3_highpart */
    case 948:  /* smulv8hi3_highpart */
    case 810:  /* umulv4hi3_highpart */
    case 809:  /* smulv4hi3_highpart */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1), 0));
      break;

    case 917:  /* cvttpd2dq */
    case 791:  /* cvttss2siq */
    case 790:  /* cvttss2si */
    case 785:  /* cvttps2pi */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XEXP (pat, 1), 0), 0, 0));
      break;

    case 928:  /* cvtss2sd */
    case 927:  /* cvtsd2ss */
    case 926:  /* cvtsi2sdq */
    case 925:  /* cvtsi2sd */
    case 787:  /* cvtsi2ssq */
    case 786:  /* cvtsi2ss */
    case 783:  /* cvtpi2ps */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0));
      break;

    case 1000:  /* sse2_punpckldq */
    case 999:  /* sse2_punpcklwd */
    case 998:  /* sse2_punpcklbw */
    case 997:  /* sse2_punpckhdq */
    case 996:  /* sse2_punpckhwd */
    case 995:  /* sse2_punpckhbw */
    case 994:  /* sse2_packuswb */
    case 993:  /* sse2_packssdw */
    case 992:  /* sse2_packsswb */
    case 991:  /* sse2_unpcklpd */
    case 990:  /* sse2_unpckhpd */
    case 848:  /* mmx_punpcklwd */
    case 847:  /* mmx_punpcklbw */
    case 845:  /* mmx_punpckhwd */
    case 844:  /* mmx_punpckhbw */
    case 843:  /* mmx_packuswb */
    case 842:  /* mmx_packssdw */
    case 841:  /* mmx_packsswb */
    case 778:  /* sse_unpcklps */
    case 777:  /* sse_unpckhps */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 904:  /* sse2_ucomi */
    case 903:  /* sse2_comi */
    case 776:  /* sse_ucomi */
    case 775:  /* sse_comi */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 902:  /* vmmaskncmpv2df3 */
    case 774:  /* vmmaskncmpv4sf3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 901:  /* vmmaskcmpv2df3 */
    case 773:  /* vmmaskcmpv4sf3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 900:  /* maskncmpv2df3 */
    case 772:  /* maskncmpv4sf3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 756:  /* *sse_nandti3_df */
    case 755:  /* *sse2_nandv2df3 */
    case 748:  /* *sse_nandsf3 */
    case 747:  /* *sse_nandv4sf3 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 760:  /* *sse2_xordf3 */
    case 759:  /* *sse2_xorv2df3 */
    case 758:  /* *sse2_iordf3 */
    case 757:  /* *sse2_iorv2df3 */
    case 754:  /* *sse2_andv2df3 */
    case 753:  /* *sse2_andv2df3 */
    case 752:  /* *sse_xorsf3 */
    case 751:  /* *sse_xorv4sf3 */
    case 750:  /* *sse_iorsf3 */
    case 749:  /* *sse_iorv4sf3 */
    case 746:  /* *sse_andsf3 */
    case 745:  /* *sse_andv4sf3 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 742:  /* vmrsqrtv4sf2 */
    case 740:  /* vmrcpv4sf2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XEXP (pat, 1), 0), 0, 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 896:  /* vmsminv2df3 */
    case 894:  /* vmsmaxv2df3 */
    case 892:  /* vmdivv2df3 */
    case 890:  /* vmmulv2df3 */
    case 888:  /* vmsubv2df3 */
    case 886:  /* vmaddv2df3 */
    case 782:  /* vmsminv4sf3 */
    case 780:  /* vmsmaxv4sf3 */
    case 738:  /* vmdivv4sf3 */
    case 736:  /* vmmulv4sf3 */
    case 734:  /* vmsubv4sf3 */
    case 732:  /* vmaddv4sf3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (pat, 1), 1);
      recog_data.dup_num[0] = 1;
      break;

    case 1019:  /* sse2_shufpd */
    case 730:  /* sse_shufps */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (pat, 1), 0, 1));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (pat, 1), 0, 2));
      break;

    case 1017:  /* sse2_movsd */
    case 1015:  /* sse2_movlpd */
    case 1014:  /* sse2_movhpd */
    case 979:  /* ashlv2di3 */
    case 978:  /* ashlv4si3 */
    case 977:  /* ashlv8hi3 */
    case 976:  /* lshrv2di3 */
    case 975:  /* lshrv4si3 */
    case 974:  /* lshrv8hi3 */
    case 973:  /* ashrv4si3 */
    case 972:  /* ashrv8hi3 */
    case 971:  /* sminv8hi3 */
    case 970:  /* uminv16qi3 */
    case 969:  /* smaxv8hi3 */
    case 968:  /* umaxv16qi3 */
    case 967:  /* gtv4si3 */
    case 966:  /* gtv8hi3 */
    case 965:  /* gtv16qi3 */
    case 964:  /* eqv4si3 */
    case 963:  /* eqv8hi3 */
    case 962:  /* eqv16qi3 */
    case 947:  /* mulv8hi3 */
    case 946:  /* ussubv8hi3 */
    case 945:  /* ussubv16qi3 */
    case 944:  /* sssubv8hi3 */
    case 943:  /* sssubv16qi3 */
    case 942:  /* subv2di3 */
    case 941:  /* subv4si3 */
    case 940:  /* subv8hi3 */
    case 939:  /* subv16qi3 */
    case 938:  /* usaddv8hi3 */
    case 937:  /* usaddv16qi3 */
    case 936:  /* ssaddv8hi3 */
    case 935:  /* ssaddv16qi3 */
    case 934:  /* addv2di3 */
    case 933:  /* addv4si3 */
    case 932:  /* addv8hi3 */
    case 931:  /* addv16qi3 */
    case 895:  /* sminv2df3 */
    case 893:  /* smaxv2df3 */
    case 891:  /* divv2df3 */
    case 889:  /* mulv2df3 */
    case 887:  /* subv2df3 */
    case 885:  /* addv2df3 */
    case 863:  /* mulv2sf3 */
    case 862:  /* pfminv2sf3 */
    case 861:  /* pfmaxv2sf3 */
    case 860:  /* eqv2sf3 */
    case 859:  /* gev2sf3 */
    case 858:  /* gtv2sf3 */
    case 856:  /* subv2sf3 */
    case 855:  /* addv2sf3 */
    case 839:  /* ashlv2si3 */
    case 838:  /* ashlv4hi3 */
    case 836:  /* lshrv2si3 */
    case 835:  /* lshrv4hi3 */
    case 834:  /* ashrv2si3 */
    case 833:  /* ashrv4hi3 */
    case 832:  /* sminv4hi3 */
    case 831:  /* uminv8qi3 */
    case 830:  /* smaxv4hi3 */
    case 829:  /* umaxv8qi3 */
    case 828:  /* gtv2si3 */
    case 827:  /* gtv4hi3 */
    case 826:  /* gtv8qi3 */
    case 825:  /* eqv2si3 */
    case 824:  /* eqv4hi3 */
    case 823:  /* eqv8qi3 */
    case 808:  /* mulv4hi3 */
    case 807:  /* ussubv4hi3 */
    case 806:  /* ussubv8qi3 */
    case 805:  /* sssubv4hi3 */
    case 804:  /* sssubv8qi3 */
    case 802:  /* subv2si3 */
    case 801:  /* subv4hi3 */
    case 800:  /* subv8qi3 */
    case 799:  /* usaddv4hi3 */
    case 798:  /* usaddv8qi3 */
    case 797:  /* ssaddv4hi3 */
    case 796:  /* ssaddv8qi3 */
    case 794:  /* addv2si3 */
    case 793:  /* addv4hi3 */
    case 792:  /* addv8qi3 */
    case 781:  /* sminv4sf3 */
    case 779:  /* smaxv4sf3 */
    case 768:  /* sse2_xorv2di3 */
    case 767:  /* *sse2_xorti3 */
    case 766:  /* sse2_iorv2di3 */
    case 765:  /* *sse2_iorti3 */
    case 762:  /* sse2_andv2di3 */
    case 761:  /* *sse2_andti3 */
    case 737:  /* divv4sf3 */
    case 735:  /* mulv4sf3 */
    case 733:  /* subv4sf3 */
    case 731:  /* addv4sf3 */
    case 728:  /* sse_movss */
    case 726:  /* sse_movlps */
    case 725:  /* sse_movhps */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 1002:  /* sse2_punpckhqdq */
    case 987:  /* ashlv2di3_ti */
    case 986:  /* ashlv4si3_ti */
    case 985:  /* ashlv8hi3_ti */
    case 984:  /* lshrv2di3_ti */
    case 983:  /* lshrv4si3_ti */
    case 982:  /* lshrv8hi3_ti */
    case 981:  /* ashrv4si3_ti */
    case 980:  /* ashrv8hi3_ti */
    case 846:  /* mmx_punpckhdq */
    case 724:  /* sse_movlhps */
    case 723:  /* sse_movhlps */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 908:  /* sse2_maskmovdqu_rex64 */
    case 907:  /* sse2_maskmovdqu */
    case 720:  /* mmx_maskmovq_rex */
    case 719:  /* mmx_maskmovq */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (pat, 1), 0, 1));
      break;

    case 1016:  /* sse2_loadsd_1 */
    case 898:  /* vmsqrtv2df2 */
    case 849:  /* mmx_punpckldq */
    case 764:  /* sse2_nandv2di3 */
    case 763:  /* *sse2_nandti3 */
    case 744:  /* vmsqrtv4sf2 */
    case 727:  /* sse_loadss_1 */
    case 679:  /* *call_value_1_rex64 */
    case 678:  /* *call_value_1 */
    case 677:  /* *call_value_0_rex64 */
    case 676:  /* *call_value_0 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 675:  /* *call_value_pop_1 */
    case 674:  /* *call_value_pop_0 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1));
      break;

    case 673:  /* allocate_stack_worker_rex64 */
    case 672:  /* allocate_stack_worker_1 */
      ro[0] = *(ro_loc[0] = &XVECEXP (XVECEXP (pat, 0, 0), 0, 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 2), 0);
      recog_data.dup_num[0] = 0;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 0;
      break;

    case 671:  /* *sse_movdfcc_const0_4 */
    case 670:  /* *sse_movdfcc_const0_3 */
    case 669:  /* *sse_movdfcc_const0_2 */
    case 668:  /* *sse_movdfcc_const0_1 */
    case 667:  /* *sse_movsfcc_const0_4 */
    case 666:  /* *sse_movsfcc_const0_3 */
    case 665:  /* *sse_movsfcc_const0_2 */
    case 664:  /* *sse_movsfcc_const0_1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 2));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 663:  /* sse_movdfcc_eq */
    case 661:  /* sse_movsfcc_eq */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 2));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 662:  /* sse_movdfcc */
    case 660:  /* sse_movsfcc */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 2));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 657:  /* *maxdf_sse */
    case 654:  /* *maxsf_sse */
    case 651:  /* *mindf_sse */
    case 648:  /* *minsf_sse */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (pat, 1), 1);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (pat, 1), 2);
      recog_data.dup_num[1] = 2;
      break;

    case 656:  /* *maxdf_nonieee */
    case 655:  /* *maxdf */
    case 653:  /* *maxsf_nonieee */
    case 652:  /* *maxsf */
    case 650:  /* *mindf_nonieee */
    case 649:  /* *mindf */
    case 647:  /* *minsf_nonieee */
    case 646:  /* *minsf */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 2);
      recog_data.dup_num[1] = 2;
      break;

    case 645:  /* *movtfcc_1 */
    case 644:  /* *movxfcc_1 */
    case 643:  /* *movdfcc_1_rex64 */
    case 642:  /* *movdfcc_1 */
    case 641:  /* *movsfcc_1 */
    case 640:  /* *movhicc_noc */
    case 639:  /* *movsicc_noc */
    case 637:  /* *movdicc_c_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 2));
      break;

    case 635:  /* strlenqi_rex_1 */
    case 634:  /* strlenqi_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 2));
      ro[4] = *(ro_loc[4] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 3));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0), 0));
      break;

    case 633:  /* cmpstrqi_rex_1 */
    case 632:  /* cmpstrqi_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 4), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 5), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 6), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      break;

    case 631:  /* cmpstrqi_nz_rex_1 */
    case 630:  /* cmpstrqi_nz_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 4), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 5), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 6), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 629:  /* rep_stosqi_rex64 */
    case 628:  /* rep_stosqi */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 4), 0);
      recog_data.dup_num[0] = 4;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 2), 0), 0);
      recog_data.dup_num[1] = 3;
      break;

    case 627:  /* rep_stossi_rex64 */
    case 626:  /* rep_stossi */
    case 625:  /* rep_stosdi_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 4), 0);
      recog_data.dup_num[0] = 4;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 2), 0), 0);
      recog_data.dup_num[1] = 3;
      break;

    case 624:  /* strsetqi_rex_1 */
    case 623:  /* strsetqi_1 */
    case 622:  /* strsethi_rex_1 */
    case 621:  /* strsethi_1 */
    case 620:  /* strsetsi_rex_1 */
    case 619:  /* strsetsi_1 */
    case 618:  /* strsetdi_rex_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 617:  /* rep_movqi_rex64 */
    case 616:  /* rep_movqi */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 4), 0);
      recog_data.dup_num[0] = 5;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 3), 0), 0);
      recog_data.dup_num[1] = 3;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 3), 1), 0);
      recog_data.dup_num[2] = 4;
      recog_data.dup_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 1);
      recog_data.dup_num[3] = 5;
      break;

    case 615:  /* rep_movsi_rex64 */
    case 614:  /* rep_movsi */
    case 613:  /* rep_movdi_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 4), 0);
      recog_data.dup_num[0] = 5;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 3), 0), 0);
      recog_data.dup_num[1] = 3;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 3), 1), 0);
      recog_data.dup_num[2] = 4;
      recog_data.dup_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0), 0);
      recog_data.dup_num[3] = 5;
      break;

    case 612:  /* strmovqi_rex_1 */
    case 611:  /* strmovqi_1 */
    case 610:  /* strmovhi_rex_1 */
    case 609:  /* strmovhi_1 */
    case 608:  /* strmovsi_rex_1 */
    case 607:  /* strmovsi_1 */
    case 606:  /* strmovdi_rex_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 2), 1), 0);
      recog_data.dup_num[0] = 3;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[1] = 2;
      break;

    case 924:  /* cvttsd2siq */
    case 923:  /* cvttsd2si */
    case 602:  /* *cosextendsfdf2 */
    case 597:  /* *sinextendsfdf2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 0));
      break;

    case 1033:  /* lddqu */
    case 1032:  /* movsldup */
    case 1031:  /* movshdup */
    case 1006:  /* sse2_movdqu */
    case 1005:  /* sse2_movdqa */
    case 1004:  /* sse2_movupd */
    case 1003:  /* sse2_movapd */
    case 919:  /* cvttpd2pi */
    case 914:  /* cvttps2dq */
    case 911:  /* sse2_movntsi */
    case 910:  /* sse2_movntv2di */
    case 909:  /* sse2_movntv2df */
    case 906:  /* sse2_pmovmskb */
    case 905:  /* sse2_movmskpd */
    case 876:  /* pfrsqrtv2sf2 */
    case 873:  /* pfrcpv2sf2 */
    case 741:  /* rsqrtv4sf2 */
    case 739:  /* rcpv4sf2 */
    case 722:  /* sse_movntdi */
    case 721:  /* sse_movntv4sf */
    case 718:  /* mmx_pmovmskb */
    case 717:  /* sse_movmskps */
    case 716:  /* *sse_movups_1 */
    case 715:  /* *sse_movaps_1 */
    case 604:  /* costf2 */
    case 603:  /* cosxf2 */
    case 601:  /* cossf2 */
    case 600:  /* cosdf2 */
    case 599:  /* sintf2 */
    case 598:  /* sinxf2 */
    case 596:  /* sinsf2 */
    case 595:  /* sindf2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      break;

    case 581:  /* *fop_tf_7 */
    case 580:  /* *fop_xf_7 */
    case 577:  /* *fop_tf_5 */
    case 576:  /* *fop_xf_5 */
    case 573:  /* *fop_tf_3 */
    case 572:  /* *fop_xf_3 */
    case 567:  /* *fop_df_5 */
    case 565:  /* *fop_df_3 */
    case 560:  /* *fop_sf_3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (pat, 1));
      break;

    case 579:  /* *fop_tf_6 */
    case 578:  /* *fop_xf_6 */
    case 575:  /* *fop_tf_4 */
    case 574:  /* *fop_xf_4 */
    case 571:  /* *fop_tf_2 */
    case 570:  /* *fop_xf_2 */
    case 566:  /* *fop_df_4 */
    case 564:  /* *fop_df_2 */
    case 559:  /* *fop_sf_2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (pat, 1));
      break;

    case 899:  /* maskcmpv2df3 */
    case 771:  /* maskcmpv4sf3 */
    case 569:  /* *fop_tf_1 */
    case 568:  /* *fop_xf_1 */
    case 563:  /* *fop_df_1_sse */
    case 562:  /* *fop_df_1 */
    case 561:  /* *fop_df_1_nosse */
    case 558:  /* *fop_sf_1_sse */
    case 557:  /* *fop_sf_1 */
    case 556:  /* *fop_sf_1_nosse */
    case 555:  /* *fop_tf_comm */
    case 554:  /* *fop_xf_comm */
    case 553:  /* *fop_df_comm_sse */
    case 552:  /* *fop_df_comm */
    case 551:  /* *fop_df_comm_nosse */
    case 550:  /* *fop_sf_comm_sse */
    case 549:  /* *fop_sf_comm */
    case 548:  /* *fop_sf_comm_nosse */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (pat, 1));
      break;

    case 547:  /* *add_tp_di */
    case 546:  /* *add_tp_si */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      break;

    case 543:  /* *tls_local_dynamic_32_once */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0, 1));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0), 0, 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 541:  /* *tls_local_dynamic_base_32_sun */
    case 540:  /* *tls_local_dynamic_base_32_gnu */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 539:  /* *tls_global_dynamic_64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XVECEXP (pat, 0, 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      break;

    case 538:  /* *tls_global_dynamic_32_sun */
    case 537:  /* *tls_global_dynamic_32_gnu */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      ro[3] = *(ro_loc[3] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 2));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 536:  /* ffssi_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      recog_data.dup_loc[0] = &XVECEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0, 0);
      recog_data.dup_num[0] = 1;
      break;

    case 529:  /* return_indirect_internal */
    case 528:  /* return_pop_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 864:  /* femms */
    case 850:  /* emms */
    case 680:  /* trap */
    case 605:  /* cld */
    case 535:  /* leave_rex64 */
    case 534:  /* leave */
    case 530:  /* nop */
    case 527:  /* return_internal */
      break;

    case 1020:  /* sse2_clflush */
    case 851:  /* ldmxcsr */
    case 533:  /* eh_return_di */
    case 532:  /* eh_return_si */
    case 526:  /* blockage */
      ro[0] = *(ro_loc[0] = &XVECEXP (pat, 0, 0));
      break;

    case 522:  /* *call_pop_1 */
    case 521:  /* *call_pop_0 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1));
      break;

    case 520:  /* doloop_end_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 519:  /* *tablejump_1_rtx64 */
    case 518:  /* *tablejump_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 0), 0));
      break;

    case 517:  /* *indirect_jump_rtx64 */
    case 516:  /* *indirect_jump */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 1));
      break;

    case 515:  /* jump */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 514:  /* *fp_jcc_6 */
    case 512:  /* *fp_jcc_4 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 513:  /* *fp_jcc_5 */
    case 511:  /* *fp_jcc_3 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 510:  /* *fp_jcc_2_sse_only */
    case 509:  /* *fp_jcc_2_sse */
    case 508:  /* *fp_jcc_2 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 2), 0));
      break;

    case 507:  /* *fp_jcc_1_sse_only */
    case 506:  /* *fp_jcc_1_sse */
    case 505:  /* *fp_jcc_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      break;

    case 504:  /* *jcc_2 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 2), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 503:  /* *jcc_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 502:  /* *sse_setccdf */
    case 501:  /* *sse_setccsf */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 430:  /* x86_shrd_1 */
    case 412:  /* x86_shld_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0);
      recog_data.dup_num[0] = 0;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 455:  /* lshrdi3_1 */
    case 428:  /* ashrdi3_1 */
    case 410:  /* ashldi3_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 403:  /* *one_cmplsi2_2_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 355:  /* *negsi2_cmpz_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 353:  /* *negsi2_1_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      break;

    case 407:  /* *one_cmplqi2_2 */
    case 405:  /* *one_cmplhi2_2 */
    case 402:  /* *one_cmplsi2_2 */
    case 399:  /* *one_cmpldi2_2_rex64 */
    case 359:  /* *negqi2_cmpz */
    case 357:  /* *neghi2_cmpz */
    case 354:  /* *negsi2_cmpz */
    case 351:  /* *negdi2_cmpz_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 464:  /* *lshrsi3_cmp_zext */
    case 462:  /* *lshrsi3_cmp_one_bit_zext */
    case 333:  /* *xorsi_2_zext_imm */
    case 311:  /* *iorsi_2_zext_imm */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 542:  /* *tls_local_dynamic_base_64 */
    case 458:  /* *lshrsi3_1_one_bit_zext */
    case 330:  /* *xorsi_1_zext_imm */
    case 308:  /* *iorsi_1_zext_imm */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      break;

    case 348:  /* *xorqi_cc_ext_1_rex64 */
    case 347:  /* *xorqi_cc_ext_1 */
    case 299:  /* *andqi_ext_0_cc */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 345:  /* *xorqi_2_slp */
    case 319:  /* *iorqi_2_slp */
    case 297:  /* *andqi_2_slp */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 0), 0);
      recog_data.dup_num[0] = 0;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[1] = 0;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[2] = 1;
      break;

    case 285:  /* *testqi_ext_3_rex64 */
    case 284:  /* *testqi_ext_3 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 2));
      break;

    case 283:  /* *testqi_ext_2 */
    case 282:  /* *testqi_ext_1_rex64 */
    case 281:  /* *testqi_ext_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 1), 0));
      break;

    case 280:  /* *testqi_ext_0 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 274:  /* *udivmodsi4_noext */
    case 272:  /* *udivmoddi4_noext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 2), 0);
      recog_data.dup_num[0] = 3;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[1] = 1;
      recog_data.dup_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[2] = 2;
      break;

    case 273:  /* udivmodsi4 */
    case 271:  /* udivmoddi4 */
    case 270:  /* divmodhi4 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 275:  /* *udivmodhi_noext */
    case 269:  /* *divmodsi_noext */
    case 266:  /* *divmoddi_noext_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 268:  /* *divmodsi4_cltd */
    case 267:  /* *divmodsi4_nocltd */
    case 265:  /* *divmoddi4_cltd_rex64 */
    case 264:  /* *divmoddi4_nocltd_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 2;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 3;
      break;

    case 261:  /* *smulsi3_highpart_zext */
    case 258:  /* *umulsi3_highpart_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 260:  /* *smulsi3_highpart_insn */
    case 259:  /* *smuldi3_highpart_rex64 */
    case 257:  /* *umulsi3_highpart_insn */
    case 256:  /* *umuldi3_highpart_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 255:  /* *mulsidi3_insn */
    case 254:  /* *mulditi3_insn */
    case 253:  /* *umulsidi3_insn */
    case 252:  /* *umulditi3_insn */
    case 251:  /* *mulqihi3_insn */
    case 250:  /* *umulqihi3_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      break;

    case 237:  /* *subsi_3_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 231:  /* subsi3_carry_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 1));
      break;

    case 244:  /* *subqi_3 */
    case 240:  /* *subhi_3 */
    case 236:  /* *subsi_3 */
    case 229:  /* *subdi_3_rex63 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 230:  /* subsi3_carry */
    case 226:  /* subdi3_carry_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 1));
      break;

    case 343:  /* *xorqi_ext_2 */
    case 342:  /* *xorqi_ext_1_rex64 */
    case 341:  /* *xorqi_ext_1 */
    case 324:  /* *iorqi_ext_2 */
    case 323:  /* *iorqi_ext_1_rex64 */
    case 322:  /* *iorqi_ext_1 */
    case 302:  /* *andqi_ext_2 */
    case 301:  /* *andqi_ext_1_rex64 */
    case 300:  /* *andqi_ext_1 */
    case 224:  /* *addqi_ext_2 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      break;

    case 340:  /* xorqi_ext_0 */
    case 321:  /* iorqi_ext_0 */
    case 298:  /* andqi_ext_0 */
    case 223:  /* *addqi_ext_1_rex64 */
    case 222:  /* addqi_ext_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      break;

    case 498:  /* *rotrqi3_1_slp */
    case 496:  /* *rotrqi3_1_one_bit_slp */
    case 485:  /* *rotlqi3_1_slp */
    case 483:  /* *rotlqi3_1_one_bit_slp */
    case 472:  /* *lshrqi3_1_slp */
    case 470:  /* *lshrqi3_1_one_bit_slp */
    case 448:  /* *ashrqi3_1_slp */
    case 446:  /* *ashrqi3_1_one_bit_slp */
    case 339:  /* *xorqi_1_slp */
    case 317:  /* *iorqi_1_slp */
    case 295:  /* *andqi_1_slp */
    case 242:  /* *subqi_1_slp */
    case 217:  /* *addqi_1_slp */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0);
      recog_data.dup_num[0] = 0;
      break;

    case 206:  /* *addsi_3_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 440:  /* *ashrsi3_cmp_zext */
    case 438:  /* *ashrsi3_one_bit_cmp_zext */
    case 416:  /* *ashlsi3_cmp_zext */
    case 332:  /* *xorsi_2_zext */
    case 310:  /* *iorsi_2_zext */
    case 291:  /* *andsi_2_zext */
    case 235:  /* *subsi_2_zext */
    case 204:  /* *addsi_2_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 492:  /* *rotrsi3_1_zext */
    case 490:  /* *rotrsi3_1_one_bit_zext */
    case 480:  /* *rotlsi3_1_zext */
    case 478:  /* *rotlsi3_1_one_bit_zext */
    case 460:  /* *lshrsi3_1_zext */
    case 436:  /* *ashrsi3_1_zext */
    case 434:  /* *ashrsi3_1_one_bit_zext */
    case 432:  /* *ashrsi3_31_zext */
    case 414:  /* *ashlsi3_1_zext */
    case 329:  /* *xorsi_1_zext */
    case 307:  /* *iorsi_1_zext */
    case 289:  /* *andsi_1_zext */
    case 247:  /* *mulsi3_1_zext */
    case 233:  /* *subsi_1_zext */
    case 202:  /* addsi_1_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      break;

    case 346:  /* *xorqi_cc_2 */
    case 337:  /* *xorhi_3 */
    case 334:  /* *xorsi_3 */
    case 327:  /* *xordi_3_rex64 */
    case 320:  /* *iorqi_3 */
    case 315:  /* *iorhi_3 */
    case 312:  /* *iorsi_3 */
    case 305:  /* *iordi_3_rex64 */
    case 221:  /* *addqi_5 */
    case 214:  /* *addhi_5 */
    case 208:  /* *addsi_5 */
    case 200:  /* *adddi_5_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      break;

    case 220:  /* *addqi_4 */
    case 213:  /* *addhi_4 */
    case 207:  /* *addsi_4 */
    case 199:  /* *adddi_4_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      break;

    case 219:  /* *addqi_3 */
    case 212:  /* *addhi_3 */
    case 205:  /* *addsi_3 */
    case 198:  /* *adddi_3_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      break;

    case 474:  /* *lshrqi2_cmp */
    case 473:  /* *lshrqi2_one_bit_cmp */
    case 468:  /* *lshrhi3_cmp */
    case 467:  /* *lshrhi3_one_bit_cmp */
    case 463:  /* *lshrsi3_cmp */
    case 461:  /* *lshrsi3_one_bit_cmp */
    case 454:  /* *lshrdi3_cmp_rex64 */
    case 453:  /* *lshrdi3_cmp_one_bit_rex64 */
    case 450:  /* *ashrqi3_cmp */
    case 449:  /* *ashrqi3_one_bit_cmp */
    case 444:  /* *ashrhi3_cmp */
    case 443:  /* *ashrhi3_one_bit_cmp */
    case 439:  /* *ashrsi3_cmp */
    case 437:  /* *ashrsi3_one_bit_cmp */
    case 427:  /* *ashrdi3_cmp_rex64 */
    case 426:  /* *ashrdi3_one_bit_cmp_rex64 */
    case 422:  /* *ashlqi3_cmp */
    case 419:  /* *ashlhi3_cmp */
    case 415:  /* *ashlsi3_cmp */
    case 409:  /* *ashldi3_cmp_rex64 */
    case 344:  /* *xorqi_cc_1 */
    case 336:  /* *xorhi_2 */
    case 331:  /* *xorsi_2 */
    case 326:  /* *xordi_2_rex64 */
    case 318:  /* *iorqi_2 */
    case 314:  /* *iorhi_2 */
    case 309:  /* *iorsi_2 */
    case 304:  /* *iordi_2_rex64 */
    case 296:  /* *andqi_2 */
    case 293:  /* *andhi_2 */
    case 290:  /* *andsi_2 */
    case 287:  /* *anddi_2 */
    case 243:  /* *subqi_2 */
    case 239:  /* *subhi_2 */
    case 234:  /* *subsi_2 */
    case 228:  /* *subdi_2_rex64 */
    case 218:  /* *addqi_2 */
    case 211:  /* *addhi_2 */
    case 203:  /* *addsi_2 */
    case 197:  /* *adddi_2_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 195:  /* *lea_general_3_zext */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 194:  /* *lea_general_3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 193:  /* *lea_general_2_zext */
    case 191:  /* *lea_general_1_zext */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 192:  /* *lea_general_2 */
    case 190:  /* *lea_general_1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 183:  /* *addsi3_carry_zext */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      break;

    case 185:  /* addqi3_cc */
    case 184:  /* *addsi3_cc */
    case 181:  /* *adddi3_cc_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[2] = *(ro_loc[2] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 182:  /* *addsi3_carry */
    case 180:  /* *adddi3_carry_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      break;

    case 659:  /* pro_epilogue_adjust_stack_rex64 */
    case 658:  /* *pro_epilogue_adjust_stack_1 */
    case 497:  /* *rotrqi3_1 */
    case 495:  /* *rotrqi3_1_one_bit */
    case 494:  /* *rotrhi3 */
    case 493:  /* *rotrhi3_one_bit */
    case 491:  /* *rotrsi3_1 */
    case 489:  /* *rotrsi3_1_one_bit */
    case 488:  /* *rotrdi3_1_rex64 */
    case 487:  /* *rotrdi3_1_one_bit_rex64 */
    case 486:  /* *rotlqi3_1 */
    case 484:  /* *rotlqi3_1_one_bit */
    case 482:  /* *rotlhi3_1 */
    case 481:  /* *rotlhi3_1_one_bit */
    case 479:  /* *rotlsi3_1 */
    case 477:  /* *rotlsi3_1_one_bit */
    case 476:  /* *rotldi3_1_rex64 */
    case 475:  /* *rotlsi3_1_one_bit_rex64 */
    case 471:  /* *lshrqi3_1 */
    case 469:  /* *lshrqi3_1_one_bit */
    case 466:  /* *lshrhi3_1 */
    case 465:  /* *lshrhi3_1_one_bit */
    case 459:  /* *lshrsi3_1 */
    case 457:  /* *lshrsi3_1_one_bit */
    case 456:  /* *lshrdi3_2 */
    case 452:  /* *lshrdi3_1_rex64 */
    case 451:  /* *lshrdi3_1_one_bit_rex64 */
    case 447:  /* *ashrqi3_1 */
    case 445:  /* *ashrqi3_1_one_bit */
    case 442:  /* *ashrhi3_1 */
    case 441:  /* *ashrhi3_1_one_bit */
    case 435:  /* *ashrsi3_1 */
    case 433:  /* *ashrsi3_1_one_bit */
    case 431:  /* ashrsi3_31 */
    case 429:  /* *ashrdi3_2 */
    case 425:  /* *ashrdi3_1_rex64 */
    case 424:  /* *ashrdi3_1_one_bit_rex64 */
    case 423:  /* ashrdi3_63_rex64 */
    case 421:  /* *ashlqi3_1 */
    case 420:  /* *ashlqi3_1_lea */
    case 418:  /* *ashlhi3_1 */
    case 417:  /* *ashlhi3_1_lea */
    case 413:  /* *ashlsi3_1 */
    case 411:  /* *ashldi3_2 */
    case 408:  /* *ashldi3_1_rex64 */
    case 338:  /* *xorqi_1 */
    case 335:  /* *xorhi_1 */
    case 328:  /* *xorsi_1 */
    case 325:  /* *xordi_1_rex64 */
    case 316:  /* *iorqi_1 */
    case 313:  /* *iorhi_1 */
    case 306:  /* *iorsi_1 */
    case 303:  /* *iordi_1_rex64 */
    case 294:  /* *andqi_1 */
    case 292:  /* *andhi_1 */
    case 288:  /* *andsi_1 */
    case 286:  /* *anddi_1_rex64 */
    case 263:  /* udivqi3 */
    case 262:  /* divqi3 */
    case 249:  /* *mulqi3_1 */
    case 248:  /* *mulhi3_1 */
    case 246:  /* *mulsi3_1 */
    case 245:  /* *muldi3_1_rex64 */
    case 241:  /* *subqi_1 */
    case 238:  /* *subhi_1 */
    case 232:  /* *subsi_1 */
    case 227:  /* *subdi_1_rex64 */
    case 225:  /* *subdi3_1 */
    case 216:  /* *addqi_1 */
    case 215:  /* *addqi_1_lea */
    case 210:  /* *addhi_1 */
    case 209:  /* *addhi_1_lea */
    case 201:  /* *addsi_1 */
    case 196:  /* *adddi_1_rex64 */
    case 179:  /* *adddi3_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      break;

    case 158:  /* fix_trunchi_memory */
    case 153:  /* fix_truncsi_memory */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 157:  /* fix_trunchi_nomemory */
    case 152:  /* fix_truncsi_nomemory */
    case 148:  /* fix_truncdi_memory */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 147:  /* fix_truncdi_nomemory */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 384:  /* *absdf2_ifs_rex64 */
    case 383:  /* absdf2_ifs */
    case 380:  /* abssf2_ifs */
    case 365:  /* *negdf2_ifs_rex64 */
    case 364:  /* negdf2_ifs */
    case 361:  /* negsf2_ifs */
    case 144:  /* *trunctfdf2_1 */
    case 142:  /* *truncxfdf2_1 */
    case 140:  /* *trunctfsf2_1 */
    case 138:  /* *truncxfsf2_1 */
    case 134:  /* *truncdfsf2_1_sse */
    case 133:  /* *truncdfsf2_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 118:  /* *extendsidi2_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 388:  /* *abstf2_if */
    case 387:  /* *absxf2_if */
    case 386:  /* *absdf2_if_rex64 */
    case 385:  /* *absdf2_if */
    case 382:  /* absdf2_memory */
    case 381:  /* *abssf2_if */
    case 379:  /* abssf2_memory */
    case 369:  /* *negtf2_if */
    case 368:  /* *negxf2_if */
    case 367:  /* *negdf2_if_rex64 */
    case 366:  /* *negdf2_if */
    case 363:  /* negdf2_memory */
    case 362:  /* *negsf2_if */
    case 360:  /* negsf2_memory */
    case 358:  /* *negqi2_1 */
    case 356:  /* *neghi2_1 */
    case 352:  /* *negsi2_1 */
    case 350:  /* *negdi2_1_rex64 */
    case 349:  /* *negdi2_1 */
    case 114:  /* zero_extendsidi2_32 */
    case 112:  /* *zero_extendqisi2_movzbw_and */
    case 111:  /* *zero_extendqisi2_and */
    case 109:  /* *zero_extendqihi2_movzbw_and */
    case 108:  /* *zero_extendqihi2_and */
    case 106:  /* zero_extendhisi2_and */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      break;

    case 74:  /* *movqi_insv_2 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 1035:  /* movddup */
    case 1012:  /* sse2_loadd */
    case 1011:  /* sse2_movq */
    case 930:  /* cvtps2pd */
    case 922:  /* cvtsd2siq */
    case 921:  /* cvtsd2si */
    case 916:  /* cvtpd2dq */
    case 915:  /* cvtdq2pd */
    case 789:  /* cvtss2siq */
    case 788:  /* cvtss2si */
    case 784:  /* cvtps2pi */
    case 594:  /* *sqrtextendsftf2 */
    case 593:  /* *sqrtextendsfxf2 */
    case 592:  /* *sqrtextenddftf2 */
    case 591:  /* *sqrtextenddfxf2 */
    case 588:  /* *sqrtextendsfdf2 */
    case 401:  /* *one_cmplsi2_1_zext */
    case 397:  /* *absextendsftf2 */
    case 396:  /* *absextenddftf2 */
    case 394:  /* *absextendsfxf2 */
    case 393:  /* *absextenddfxf2 */
    case 391:  /* *absextendsfdf2 */
    case 378:  /* *negextendsftf2 */
    case 377:  /* *negextenddftf2 */
    case 375:  /* *negextendsfxf2 */
    case 374:  /* *negextenddfxf2 */
    case 372:  /* *negextendsfdf2 */
    case 188:  /* *lea_1_zext */
    case 126:  /* *extendqisi2_zext */
    case 123:  /* *extendhisi2_zext */
    case 71:  /* *movqi_extzv_2_rex64 */
    case 70:  /* *movqi_extzv_2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 62:  /* *movstrictqi_xor */
    case 56:  /* *movstricthi_xor */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      break;

    case 105:  /* swaptf */
    case 104:  /* swapxf */
    case 95:  /* *swapdf */
    case 90:  /* *swapsf */
    case 86:  /* *swapdi_rex64 */
    case 60:  /* *swapqi */
    case 54:  /* *swaphi_2 */
    case 53:  /* *swaphi_1 */
    case 47:  /* *swapsi */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      recog_data.dup_loc[0] = &XEXP (XVECEXP (pat, 0, 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XVECEXP (pat, 0, 1), 1);
      recog_data.dup_num[1] = 0;
      break;

    case 1034:  /* loadddup */
    case 1018:  /* sse2_storesd */
    case 1013:  /* sse2_stored */
    case 1010:  /* sse2_movq2dq_rex64 */
    case 1009:  /* sse2_movq2dq */
    case 1008:  /* sse2_movdq2q_rex64 */
    case 1007:  /* sse2_movdq2q */
    case 920:  /* cvtpi2pd */
    case 918:  /* cvtpd2pi */
    case 913:  /* cvtps2dq */
    case 912:  /* cvtdq2ps */
    case 897:  /* sqrtv2df2 */
    case 880:  /* pswapdv2sf2 */
    case 879:  /* pswapdv2si2 */
    case 871:  /* floatv2si2 */
    case 865:  /* pf2id */
    case 743:  /* sqrtv4sf2 */
    case 729:  /* sse_storess */
    case 590:  /* sqrttf2 */
    case 589:  /* sqrtxf2 */
    case 587:  /* sqrtdf2_i387 */
    case 586:  /* sqrtdf2_1_sse_only */
    case 585:  /* sqrtdf2_1 */
    case 584:  /* sqrtsf2_i387 */
    case 583:  /* sqrtsf2_1_sse_only */
    case 582:  /* sqrtsf2_1 */
    case 406:  /* *one_cmplqi2_1 */
    case 404:  /* *one_cmplhi2_1 */
    case 400:  /* *one_cmplsi2_1 */
    case 398:  /* *one_cmpldi2_1_rex64 */
    case 395:  /* *abstf2_1 */
    case 392:  /* *absxf2_1 */
    case 390:  /* *absdf2_1 */
    case 389:  /* *abssf2_1 */
    case 376:  /* *negtf2_1 */
    case 373:  /* *negxf2_1 */
    case 371:  /* *negdf2_1 */
    case 370:  /* *negsf2_1 */
    case 187:  /* *lea_1_rex64 */
    case 178:  /* floatditf2 */
    case 177:  /* floatdixf2 */
    case 176:  /* floatsitf2 */
    case 175:  /* floatsixf2 */
    case 174:  /* floathitf2 */
    case 173:  /* floathixf2 */
    case 172:  /* *floatdidf2_sse */
    case 171:  /* *floatdidf2_i387 */
    case 170:  /* *floatdidf2_i387_only */
    case 169:  /* *floatsidf2_sse */
    case 168:  /* *floatsidf2_i387 */
    case 167:  /* floathidf2 */
    case 166:  /* *floatdisf2_sse */
    case 165:  /* *floatdisf2_i387 */
    case 164:  /* *floatdisf2_i387_only */
    case 163:  /* *floatsisf2_sse */
    case 162:  /* *floatsisf2_i387 */
    case 161:  /* floathisf2 */
    case 156:  /* *fix_trunchi_1 */
    case 155:  /* fix_truncdfsi_sse */
    case 154:  /* fix_truncsfsi_sse */
    case 151:  /* *fix_truncsi_1 */
    case 150:  /* fix_truncdfdi_sse */
    case 149:  /* fix_truncsfdi_sse */
    case 146:  /* *fix_truncdi_1 */
    case 145:  /* *trunctfdf2_2 */
    case 143:  /* *truncxfdf2_2 */
    case 141:  /* *trunctfsf2_2 */
    case 139:  /* *truncxfsf2_2 */
    case 137:  /* truncdfsf2_sse_only */
    case 136:  /* truncdfsf2_3 */
    case 135:  /* *truncdfsf2_2 */
    case 132:  /* *extenddftf2_1 */
    case 131:  /* *extenddfxf2_1 */
    case 130:  /* *extendsftf2_1 */
    case 129:  /* *extendsfxf2_1 */
    case 128:  /* *extendsfdf2_1_sse_only */
    case 127:  /* *extendsfdf2_1 */
    case 125:  /* extendqisi2 */
    case 124:  /* extendqihi2 */
    case 122:  /* extendhisi2 */
    case 121:  /* extendqidi2 */
    case 120:  /* extendhidi2 */
    case 119:  /* extendsidi2_rex64 */
    case 117:  /* zero_extendqidi2 */
    case 116:  /* zero_extendhidi2 */
    case 115:  /* zero_extendsidi2_rex64 */
    case 113:  /* *zero_extendqisi2_movzbw */
    case 110:  /* *zero_extendqihi2_movzbw */
    case 107:  /* *zero_extendhisi2_movzwl */
    case 85:  /* *movabsdi_2_rex64 */
    case 69:  /* *movsi_extzv_1 */
    case 68:  /* *movabsqi_2_rex64 */
    case 66:  /* *movqi_extv_1_rex64 */
    case 65:  /* *movqi_extv_1 */
    case 64:  /* *movhi_extv_1 */
    case 63:  /* *movsi_extv_1 */
    case 52:  /* *movabshi_2_rex64 */
    case 46:  /* *movabssi_2_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 525:  /* *call_1_rex64 */
    case 524:  /* *call_1 */
    case 523:  /* *call_0 */
    case 500:  /* setcc_2 */
    case 84:  /* *movabsdi_1_rex64 */
    case 73:  /* *movsi_insv_1_rex64 */
    case 72:  /* movsi_insv_1 */
    case 67:  /* *movabsqi_1_rex64 */
    case 61:  /* *movstrictqi_1 */
    case 55:  /* *movstricthi_1 */
    case 51:  /* *movabshi_1_rex64 */
    case 45:  /* *movabssi_1_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      break;

    case 638:  /* x86_movsicc_0_m1 */
    case 636:  /* x86_movdicc_0_m1_rex64 */
    case 531:  /* set_got */
    case 79:  /* popdi1 */
    case 78:  /* *popdi1_epilogue_rex64 */
    case 41:  /* popsi1 */
    case 40:  /* *popsi1_epilogue */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      break;

    case 81:  /* *movdi_or_rex64 */
    case 80:  /* *movdi_xor_rex64 */
    case 77:  /* *pushdi2_prologue_rex64 */
    case 43:  /* *movsi_or */
    case 42:  /* *movsi_xor */
    case 39:  /* *pushsi2_prologue */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      break;

    case 884:  /* *prefetch_3dnow_rex */
    case 883:  /* *prefetch_3dnow */
    case 714:  /* *movti_rex64 */
    case 713:  /* movti_internal */
    case 712:  /* *pushv2sf */
    case 711:  /* *pushv8qi */
    case 710:  /* *pushv4hi */
    case 709:  /* *pushv2si */
    case 708:  /* *pushv4si */
    case 707:  /* *pushv4sf */
    case 706:  /* *pushv16qi */
    case 705:  /* *pushv8hi */
    case 704:  /* *pushv2di */
    case 703:  /* *pushv2df */
    case 702:  /* *pushti */
    case 701:  /* *pushv2sf */
    case 700:  /* *pushv8qi */
    case 699:  /* *pushv4hi */
    case 698:  /* *pushv2si */
    case 697:  /* *pushv4si */
    case 696:  /* *pushv4sf */
    case 695:  /* *pushv16qi */
    case 694:  /* *pushv8hi */
    case 693:  /* *pushv2di */
    case 692:  /* *pushv2df */
    case 691:  /* movv16qi_internal */
    case 690:  /* movv8hi_internal */
    case 689:  /* movv2df_internal */
    case 688:  /* movv2sf_internal */
    case 687:  /* movv2si_internal */
    case 686:  /* movv4hi_internal */
    case 685:  /* movv8qi_internal */
    case 684:  /* movv2di_internal */
    case 683:  /* movv4si_internal */
    case 682:  /* movv4sf_internal */
    case 681:  /* *conditional_trap_1 */
    case 499:  /* *setcc_1 */
    case 189:  /* *lea_2_rex64 */
    case 186:  /* *lea_1 */
    case 103:  /* *movtf_integer */
    case 102:  /* *movxf_integer */
    case 101:  /* *movtf_nointeger */
    case 100:  /* *movxf_nointeger */
    case 99:  /* *pushtf_integer */
    case 98:  /* *pushxf_integer */
    case 97:  /* *pushtf_nointeger */
    case 96:  /* *pushxf_nointeger */
    case 94:  /* *movdf_integer */
    case 93:  /* *movdf_nointeger */
    case 92:  /* *pushdf_integer */
    case 91:  /* *pushdf_nointeger */
    case 89:  /* *movsf_1 */
    case 88:  /* *pushsf_rex64 */
    case 87:  /* *pushsf */
    case 83:  /* *movdi_1_rex64 */
    case 82:  /* *movdi_2 */
    case 76:  /* pushdi2_rex64 */
    case 75:  /* *pushdi */
    case 59:  /* *movqi_1 */
    case 58:  /* *pushqi2_rex64 */
    case 57:  /* *pushqi2 */
    case 50:  /* *movhi_1 */
    case 49:  /* *pushhi2_rex64 */
    case 48:  /* *pushhi2 */
    case 44:  /* *movsi_1 */
    case 38:  /* *pushsi2_rex64 */
    case 37:  /* *pushsi2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      break;

    case 160:  /* x86_fldcw_1 */
    case 30:  /* x86_sahf_1 */
      ro[0] = *(ro_loc[0] = &XVECEXP (XEXP (pat, 1), 0, 0));
      break;

    case 953:  /* sse2_clrti */
    case 852:  /* stmxcsr */
    case 814:  /* mmx_clrdi */
    case 770:  /* sse_clrv2df */
    case 769:  /* sse_clrv4sf */
    case 545:  /* *load_tp_di */
    case 544:  /* *load_tp_si */
    case 159:  /* x86_fnstcw_1 */
    case 29:  /* x86_fnstsw_1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      break;

    case 840:  /* mmx_ashldi3 */
    case 837:  /* mmx_lshrdi3 */
    case 815:  /* mmx_anddi3 */
    case 813:  /* mmx_xordi3 */
    case 812:  /* mmx_iordi3 */
    case 803:  /* mmx_subdi3 */
    case 795:  /* mmx_adddi3 */
    case 28:  /* *cmpfp_2u_1 */
    case 26:  /* *cmpfp_2_tf_1 */
    case 25:  /* *cmpfp_2_xf_1 */
    case 22:  /* *cmpfp_2_df_1 */
    case 20:  /* *cmpfp_2_sf_1 */
    case 18:  /* *cmpfp_0 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (XEXP (pat, 1), 0, 0), 1));
      break;

    case 17:  /* *cmpqi_ext_4 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0));
      break;

    case 16:  /* cmpqi_ext_3_insn_rex64 */
    case 15:  /* cmpqi_ext_3_insn */
    case 14:  /* *cmpqi_ext_2 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 13:  /* *cmpqi_ext_1_rex64 */
    case 12:  /* *cmpqi_ext_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 1), 0), 0));
      break;

    case 279:  /* *testqi_1 */
    case 278:  /* *testhi_1 */
    case 277:  /* testsi_1 */
    case 276:  /* *testdi_1_rex64 */
    case 11:  /* *cmpqi_minus_1 */
    case 7:  /* *cmphi_minus_1 */
    case 4:  /* *cmpsi_minus_1 */
    case 1:  /* *cmpdi_minus_1_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 36:  /* *cmpfp_iu_sse_only */
    case 35:  /* *cmpfp_iu_sse */
    case 34:  /* *cmpfp_iu */
    case 33:  /* *cmpfp_i_sse_only */
    case 32:  /* *cmpfp_i_sse */
    case 31:  /* *cmpfp_i */
    case 27:  /* *cmpfp_2u */
    case 24:  /* *cmpfp_2_tf */
    case 23:  /* *cmpfp_2_xf */
    case 21:  /* *cmpfp_2_df */
    case 19:  /* *cmpfp_2_sf */
    case 10:  /* *cmpqi_1 */
    case 9:  /* *cmpqi_ccno_1 */
    case 8:  /* *cmphi_1 */
    case 6:  /* *cmphi_ccno_1 */
    case 5:  /* *cmpsi_1_insn */
    case 3:  /* *cmpsi_ccno_1 */
    case 2:  /* cmpdi_1_insn_rex64 */
    case 0:  /* cmpdi_ccno_1_rex64 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 1));
      break;

    default:
      abort ();
    }
}
