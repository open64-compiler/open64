/* Generated automatically by the program `genopinit'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "reload.h"

void
init_all_optabs ()
{
  if (HAVE_extendqidi2)
    extendtab[(int) DImode][(int) QImode][0] = CODE_FOR_extendqidi2;
  one_cmpl_optab->handlers[(int) SImode].insn_code = CODE_FOR_one_cmplsi2;
  neg_optab->handlers[(int) SImode].insn_code = CODE_FOR_negsi2;
  ffs_optab->handlers[(int) SImode].insn_code = CODE_FOR_ffssi2;
  and_optab->handlers[(int) SImode].insn_code = CODE_FOR_andsi3;
  rotl_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotlsi3;
  if (HAVE_extendsfdf2)
    extendtab[(int) DFmode][(int) SFmode][0] = CODE_FOR_extendsfdf2;
  if (HAVE_negdf2)
    negv_optab->handlers[(int) (int) DFmode].insn_code =
    neg_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_negdf2;
  if (HAVE_absdf2)
    absv_optab->handlers[(int) (int) DFmode].insn_code =
    abs_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_absdf2;
  if (HAVE_adddf3)
    addv_optab->handlers[(int) (int) DFmode].insn_code =
    add_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_adddf3;
  if (HAVE_subdf3)
    subv_optab->handlers[(int) (int) DFmode].insn_code =
    sub_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_subdf3;
  if (HAVE_muldf3)
    smulv_optab->handlers[(int) (int) DFmode].insn_code =
    smul_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_muldf3;
  if (HAVE_divdf3)
    sdiv_optab->handlers[(int) DFmode].insn_code = CODE_FOR_divdf3;
  if (HAVE_sqrtdf2)
    sqrt_optab->handlers[(int) DFmode].insn_code = CODE_FOR_sqrtdf2;
  if (HAVE_floatdidf2)
    floattab[(int) DFmode][(int) DImode][0] = CODE_FOR_floatdidf2;
  if (HAVE_fix_truncdfdi2)
    fixtrunctab[(int) DFmode][(int) DImode][0] = CODE_FOR_fix_truncdfdi2;
  if (HAVE_one_cmpldi2)
    one_cmpl_optab->handlers[(int) DImode].insn_code = CODE_FOR_one_cmpldi2;
  if (HAVE_absdi2)
    abs_optab->handlers[(int) DImode].insn_code = CODE_FOR_absdi2;
  if (HAVE_ffsdi2)
    ffs_optab->handlers[(int) DImode].insn_code = CODE_FOR_ffsdi2;
  if (HAVE_muldi3)
    smul_optab->handlers[(int) DImode].insn_code = CODE_FOR_muldi3;
  if (HAVE_smuldi3_highpart)
    smul_highpart_optab->handlers[(int) DImode].insn_code = CODE_FOR_smuldi3_highpart;
  if (HAVE_umuldi3_highpart)
    umul_highpart_optab->handlers[(int) DImode].insn_code = CODE_FOR_umuldi3_highpart;
  if (HAVE_udivdi3)
    udiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivdi3;
  if (HAVE_rotldi3)
    rotl_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotldi3;
  if (HAVE_anddi3)
    and_optab->handlers[(int) DImode].insn_code = CODE_FOR_anddi3;
  if (HAVE_extenddftf2)
    extendtab[(int) TFmode][(int) DFmode][0] = CODE_FOR_extenddftf2;
  if (HAVE_extendsftf2)
    extendtab[(int) TFmode][(int) SFmode][0] = CODE_FOR_extendsftf2;
  if (HAVE_floatditf2)
    floattab[(int) TFmode][(int) DImode][0] = CODE_FOR_floatditf2;
  if (HAVE_floatsitf2)
    floattab[(int) TFmode][(int) SImode][0] = CODE_FOR_floatsitf2;
  if (HAVE_fix_trunctfdi2)
    fixtrunctab[(int) TFmode][(int) DImode][0] = CODE_FOR_fix_trunctfdi2;
  if (HAVE_fix_trunctfsi2)
    fixtrunctab[(int) TFmode][(int) SImode][0] = CODE_FOR_fix_trunctfsi2;
  if (HAVE_negtf2)
    negv_optab->handlers[(int) (int) TFmode].insn_code =
    neg_optab->handlers[(int) (int) TFmode].insn_code = CODE_FOR_negtf2;
  if (HAVE_abstf2)
    absv_optab->handlers[(int) (int) TFmode].insn_code =
    abs_optab->handlers[(int) (int) TFmode].insn_code = CODE_FOR_abstf2;
  if (HAVE_addv16qi3)
    add_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_addv16qi3;
  if (HAVE_addv8hi3)
    add_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_addv8hi3;
  if (HAVE_addv4si3)
    add_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_addv4si3;
  if (HAVE_addv4sf3)
    addv_optab->handlers[(int) (int) V4SFmode].insn_code =
    add_optab->handlers[(int) (int) V4SFmode].insn_code = CODE_FOR_addv4sf3;
  if (HAVE_andv4si3)
    and_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_andv4si3;
  if (HAVE_umaxv16qi3)
    umax_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_umaxv16qi3;
  if (HAVE_smaxv16qi3)
    smax_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_smaxv16qi3;
  if (HAVE_umaxv8hi3)
    umax_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_umaxv8hi3;
  if (HAVE_smaxv8hi3)
    smax_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_smaxv8hi3;
  if (HAVE_umaxv4si3)
    umax_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_umaxv4si3;
  if (HAVE_smaxv4si3)
    smax_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_smaxv4si3;
  if (HAVE_uminv16qi3)
    umin_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_uminv16qi3;
  if (HAVE_sminv16qi3)
    smin_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_sminv16qi3;
  if (HAVE_uminv8hi3)
    umin_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_uminv8hi3;
  if (HAVE_sminv8hi3)
    smin_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_sminv8hi3;
  if (HAVE_uminv4si3)
    umin_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_uminv4si3;
  if (HAVE_sminv4si3)
    smin_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_sminv4si3;
  if (HAVE_iorv4si3)
    ior_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_iorv4si3;
  if (HAVE_subv16qi3)
    sub_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_subv16qi3;
  if (HAVE_subv8hi3)
    sub_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_subv8hi3;
  if (HAVE_subv4si3)
    sub_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_subv4si3;
  if (HAVE_subv4sf3)
    subv_optab->handlers[(int) (int) V4SFmode].insn_code =
    sub_optab->handlers[(int) (int) V4SFmode].insn_code = CODE_FOR_subv4sf3;
  if (HAVE_xorv4si3)
    xor_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_xorv4si3;
  if (HAVE_ftruncv4sf2)
    ftrunc_optab->handlers[(int) V4SFmode].insn_code = CODE_FOR_ftruncv4sf2;
  if (HAVE_absv16qi2)
    abs_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_absv16qi2;
  if (HAVE_absv8hi2)
    abs_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_absv8hi2;
  if (HAVE_absv4si2)
    abs_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_absv4si2;
  if (HAVE_absv4sf2)
    absv_optab->handlers[(int) (int) V4SFmode].insn_code =
    abs_optab->handlers[(int) (int) V4SFmode].insn_code = CODE_FOR_absv4sf2;
  if (HAVE_zero_extendqidi2)
    extendtab[(int) DImode][(int) QImode][1] = CODE_FOR_zero_extendqidi2;
  if (HAVE_zero_extendhidi2)
    extendtab[(int) DImode][(int) HImode][1] = CODE_FOR_zero_extendhidi2;
  if (HAVE_extendhidi2)
    extendtab[(int) DImode][(int) HImode][0] = CODE_FOR_extendhidi2;
  if (HAVE_zero_extendsidi2)
    extendtab[(int) DImode][(int) SImode][1] = CODE_FOR_zero_extendsidi2;
  if (HAVE_extendsidi2)
    extendtab[(int) DImode][(int) SImode][0] = CODE_FOR_extendsidi2;
  extendtab[(int) SImode][(int) QImode][1] = CODE_FOR_zero_extendqisi2;
  extendtab[(int) SImode][(int) QImode][0] = CODE_FOR_extendqisi2;
  extendtab[(int) HImode][(int) QImode][1] = CODE_FOR_zero_extendqihi2;
  extendtab[(int) HImode][(int) QImode][0] = CODE_FOR_extendqihi2;
  extendtab[(int) SImode][(int) HImode][1] = CODE_FOR_zero_extendhisi2;
  extendtab[(int) SImode][(int) HImode][0] = CODE_FOR_extendhisi2;
  add_optab->handlers[(int) SImode].insn_code = CODE_FOR_addsi3;
  sub_optab->handlers[(int) SImode].insn_code = CODE_FOR_subsi3;
  if (HAVE_sminsi3)
    smin_optab->handlers[(int) SImode].insn_code = CODE_FOR_sminsi3;
  if (HAVE_smaxsi3)
    smax_optab->handlers[(int) SImode].insn_code = CODE_FOR_smaxsi3;
  if (HAVE_uminsi3)
    umin_optab->handlers[(int) SImode].insn_code = CODE_FOR_uminsi3;
  if (HAVE_umaxsi3)
    umax_optab->handlers[(int) SImode].insn_code = CODE_FOR_umaxsi3;
  abs_optab->handlers[(int) SImode].insn_code = CODE_FOR_abssi2;
  smul_optab->handlers[(int) SImode].insn_code = CODE_FOR_mulsi3;
  if (HAVE_divmodsi4)
    sdivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_divmodsi4;
  if (HAVE_udivsi3)
    udiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivsi3;
  sdiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_divsi3;
  smod_optab->handlers[(int) SImode].insn_code = CODE_FOR_modsi3;
  udivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivmodsi4;
  ior_optab->handlers[(int) SImode].insn_code = CODE_FOR_iorsi3;
  xor_optab->handlers[(int) SImode].insn_code = CODE_FOR_xorsi3;
  ashl_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashlsi3;
  lshr_optab->handlers[(int) SImode].insn_code = CODE_FOR_lshrsi3;
  ashr_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashrsi3;
  if (HAVE_negsf2)
    negv_optab->handlers[(int) (int) SFmode].insn_code =
    neg_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_negsf2;
  if (HAVE_abssf2)
    absv_optab->handlers[(int) (int) SFmode].insn_code =
    abs_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_abssf2;
  if (HAVE_addsf3)
    addv_optab->handlers[(int) (int) SFmode].insn_code =
    add_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_addsf3;
  if (HAVE_subsf3)
    subv_optab->handlers[(int) (int) SFmode].insn_code =
    sub_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_subsf3;
  if (HAVE_mulsf3)
    smulv_optab->handlers[(int) (int) SFmode].insn_code =
    smul_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_mulsf3;
  if (HAVE_divsf3)
    sdiv_optab->handlers[(int) SFmode].insn_code = CODE_FOR_divsf3;
  if (HAVE_sqrtsf2)
    sqrt_optab->handlers[(int) SFmode].insn_code = CODE_FOR_sqrtsf2;
  if (HAVE_maxsf3)
    smax_optab->handlers[(int) SFmode].insn_code = CODE_FOR_maxsf3;
  if (HAVE_minsf3)
    smin_optab->handlers[(int) SFmode].insn_code = CODE_FOR_minsf3;
  if (HAVE_movsfcc)
    movcc_gen_code[(int) SFmode] = CODE_FOR_movsfcc;
  if (HAVE_maxdf3)
    smax_optab->handlers[(int) DFmode].insn_code = CODE_FOR_maxdf3;
  if (HAVE_mindf3)
    smin_optab->handlers[(int) DFmode].insn_code = CODE_FOR_mindf3;
  if (HAVE_movdfcc)
    movcc_gen_code[(int) DFmode] = CODE_FOR_movdfcc;
  if (HAVE_floatsidf2)
    floattab[(int) DFmode][(int) SImode][0] = CODE_FOR_floatsidf2;
  if (HAVE_floatunssidf2)
    floattab[(int) DFmode][(int) SImode][1] = CODE_FOR_floatunssidf2;
  if (HAVE_fix_truncdfsi2)
    fixtrunctab[(int) DFmode][(int) SImode][0] = CODE_FOR_fix_truncdfsi2;
  if (HAVE_floatdisf2)
    floattab[(int) SFmode][(int) DImode][0] = CODE_FOR_floatdisf2;
  if (HAVE_mulsidi3)
    smul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_mulsidi3;
  if (HAVE_umulsidi3)
    umul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_umulsidi3;
  smul_highpart_optab->handlers[(int) SImode].insn_code = CODE_FOR_smulsi3_highpart;
  if (HAVE_umulsi3_highpart)
    umul_highpart_optab->handlers[(int) SImode].insn_code = CODE_FOR_umulsi3_highpart;
  add_optab->handlers[(int) DImode].insn_code = CODE_FOR_adddi3;
  sub_optab->handlers[(int) DImode].insn_code = CODE_FOR_subdi3;
  neg_optab->handlers[(int) DImode].insn_code = CODE_FOR_negdi2;
  if (HAVE_divdi3)
    sdiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_divdi3;
  if (HAVE_moddi3)
    smod_optab->handlers[(int) DImode].insn_code = CODE_FOR_moddi3;
  if (HAVE_ashldi3)
    ashl_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashldi3;
  if (HAVE_lshrdi3)
    lshr_optab->handlers[(int) DImode].insn_code = CODE_FOR_lshrdi3;
  ashr_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashrdi3;
  if (HAVE_iordi3)
    ior_optab->handlers[(int) DImode].insn_code = CODE_FOR_iordi3;
  if (HAVE_xordi3)
    xor_optab->handlers[(int) DImode].insn_code = CODE_FOR_xordi3;
  mov_optab->handlers[(int) SImode].insn_code = CODE_FOR_movsi;
  mov_optab->handlers[(int) HImode].insn_code = CODE_FOR_movhi;
  mov_optab->handlers[(int) QImode].insn_code = CODE_FOR_movqi;
  mov_optab->handlers[(int) CCmode].insn_code = CODE_FOR_movcc;
  mov_optab->handlers[(int) SFmode].insn_code = CODE_FOR_movsf;
  mov_optab->handlers[(int) DFmode].insn_code = CODE_FOR_movdf;
  if (HAVE_movtf)
    mov_optab->handlers[(int) TFmode].insn_code = CODE_FOR_movtf;
  mov_optab->handlers[(int) DImode].insn_code = CODE_FOR_movdi;
  if (HAVE_movti)
    mov_optab->handlers[(int) TImode].insn_code = CODE_FOR_movti;
  movstr_optab[(int) SImode] = CODE_FOR_movstrsi;
  cmp_optab->handlers[(int) SImode].insn_code = CODE_FOR_cmpsi;
  if (HAVE_cmpdi)
    cmp_optab->handlers[(int) DImode].insn_code = CODE_FOR_cmpdi;
  if (HAVE_cmpsf)
    cmp_optab->handlers[(int) SFmode].insn_code = CODE_FOR_cmpsf;
  if (HAVE_cmpdf)
    cmp_optab->handlers[(int) DFmode].insn_code = CODE_FOR_cmpdf;
  if (HAVE_cmptf)
    cmp_optab->handlers[(int) TFmode].insn_code = CODE_FOR_cmptf;
  bcc_gen_fctn[(int) EQ] = gen_beq;
  bcc_gen_fctn[(int) NE] = gen_bne;
  bcc_gen_fctn[(int) GE] = gen_bge;
  bcc_gen_fctn[(int) GT] = gen_bgt;
  bcc_gen_fctn[(int) LE] = gen_ble;
  bcc_gen_fctn[(int) LT] = gen_blt;
  bcc_gen_fctn[(int) GEU] = gen_bgeu;
  bcc_gen_fctn[(int) GTU] = gen_bgtu;
  bcc_gen_fctn[(int) LEU] = gen_bleu;
  bcc_gen_fctn[(int) LTU] = gen_bltu;
  bcc_gen_fctn[(int) UNORDERED] = gen_bunordered;
  bcc_gen_fctn[(int) ORDERED] = gen_bordered;
  bcc_gen_fctn[(int) UNEQ] = gen_buneq;
  bcc_gen_fctn[(int) UNGE] = gen_bunge;
  bcc_gen_fctn[(int) UNGT] = gen_bungt;
  bcc_gen_fctn[(int) UNLE] = gen_bunle;
  bcc_gen_fctn[(int) UNLT] = gen_bunlt;
  bcc_gen_fctn[(int) LTGT] = gen_bltgt;
  setcc_gen_code[(int) EQ] = CODE_FOR_seq;
  setcc_gen_code[(int) NE] = CODE_FOR_sne;
  setcc_gen_code[(int) GT] = CODE_FOR_sgt;
  setcc_gen_code[(int) LT] = CODE_FOR_slt;
  setcc_gen_code[(int) GE] = CODE_FOR_sge;
  setcc_gen_code[(int) LE] = CODE_FOR_sle;
  setcc_gen_code[(int) GTU] = CODE_FOR_sgtu;
  setcc_gen_code[(int) LTU] = CODE_FOR_sltu;
  setcc_gen_code[(int) GEU] = CODE_FOR_sgeu;
  setcc_gen_code[(int) LEU] = CODE_FOR_sleu;
  if (HAVE_movv4si)
    mov_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_movv4si;
  if (HAVE_movv8hi)
    mov_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_movv8hi;
  if (HAVE_movv16qi)
    mov_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_movv16qi;
  if (HAVE_movv4sf)
    mov_optab->handlers[(int) V4SFmode].insn_code = CODE_FOR_movv4sf;
  if (HAVE_mulv4sf3)
    smulv_optab->handlers[(int) (int) V4SFmode].insn_code =
    smul_optab->handlers[(int) (int) V4SFmode].insn_code = CODE_FOR_mulv4sf3;
}
