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
  if (HAVE_adddf3)
    addv_optab->handlers[(int) (int) DFmode].insn_code =
    add_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_adddf3;
  if (HAVE_addsf3)
    addv_optab->handlers[(int) (int) SFmode].insn_code =
    add_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_addsf3;
  if (HAVE_subdf3)
    subv_optab->handlers[(int) (int) DFmode].insn_code =
    sub_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_subdf3;
  if (HAVE_subsf3)
    subv_optab->handlers[(int) (int) SFmode].insn_code =
    sub_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_subsf3;
  if (HAVE_smuldi3_highpart)
    smul_highpart_optab->handlers[(int) DImode].insn_code = CODE_FOR_smuldi3_highpart;
  if (HAVE_umuldi3_highpart)
    umul_highpart_optab->handlers[(int) DImode].insn_code = CODE_FOR_umuldi3_highpart;
  if (HAVE_divdf3)
    sdiv_optab->handlers[(int) DFmode].insn_code = CODE_FOR_divdf3;
  if (HAVE_divsf3)
    sdiv_optab->handlers[(int) SFmode].insn_code = CODE_FOR_divsf3;
  if (HAVE_sqrtdf2)
    sqrt_optab->handlers[(int) DFmode].insn_code = CODE_FOR_sqrtdf2;
  if (HAVE_sqrtsf2)
    sqrt_optab->handlers[(int) SFmode].insn_code = CODE_FOR_sqrtsf2;
  if (HAVE_abssi2)
    abs_optab->handlers[(int) SImode].insn_code = CODE_FOR_abssi2;
  if (HAVE_absdi2)
    abs_optab->handlers[(int) DImode].insn_code = CODE_FOR_absdi2;
  if (HAVE_absdf2)
    absv_optab->handlers[(int) (int) DFmode].insn_code =
    abs_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_absdf2;
  if (HAVE_abssf2)
    absv_optab->handlers[(int) (int) SFmode].insn_code =
    abs_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_abssf2;
  if (HAVE_ffssi2)
    ffs_optab->handlers[(int) SImode].insn_code = CODE_FOR_ffssi2;
  if (HAVE_ffsdi2)
    ffs_optab->handlers[(int) DImode].insn_code = CODE_FOR_ffsdi2;
  neg_optab->handlers[(int) SImode].insn_code = CODE_FOR_negsi2;
  if (HAVE_negdf2)
    negv_optab->handlers[(int) (int) DFmode].insn_code =
    neg_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_negdf2;
  if (HAVE_negsf2)
    negv_optab->handlers[(int) (int) SFmode].insn_code =
    neg_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_negsf2;
  one_cmpl_optab->handlers[(int) SImode].insn_code = CODE_FOR_one_cmplsi2;
  one_cmpl_optab->handlers[(int) DImode].insn_code = CODE_FOR_one_cmpldi2;
  if (HAVE_extendsfdf2)
    extendtab[(int) DFmode][(int) SFmode][0] = CODE_FOR_extendsfdf2;
  if (HAVE_fix_truncdfdi2)
    fixtrunctab[(int) DFmode][(int) DImode][0] = CODE_FOR_fix_truncdfdi2;
  if (HAVE_fix_truncsfdi2)
    fixtrunctab[(int) SFmode][(int) DImode][0] = CODE_FOR_fix_truncsfdi2;
  if (HAVE_floatsidf2)
    floattab[(int) DFmode][(int) SImode][0] = CODE_FOR_floatsidf2;
  if (HAVE_floatdidf2)
    floattab[(int) DFmode][(int) DImode][0] = CODE_FOR_floatdidf2;
  if (HAVE_floatsisf2)
    floattab[(int) SFmode][(int) SImode][0] = CODE_FOR_floatsisf2;
  if (HAVE_floatdisf2)
    floattab[(int) SFmode][(int) DImode][0] = CODE_FOR_floatdisf2;
  if (HAVE_movcc)
    mov_optab->handlers[(int) CCmode].insn_code = CODE_FOR_movcc;
  if (HAVE_rotrsi3)
    rotr_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotrsi3;
  if (HAVE_rotrdi3)
    rotr_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotrdi3;
  add_optab->handlers[(int) SImode].insn_code = CODE_FOR_addsi3;
  if (HAVE_adddi3)
    add_optab->handlers[(int) DImode].insn_code = CODE_FOR_adddi3;
  sub_optab->handlers[(int) SImode].insn_code = CODE_FOR_subsi3;
  if (HAVE_subdi3)
    sub_optab->handlers[(int) DImode].insn_code = CODE_FOR_subdi3;
  if (HAVE_muldf3)
    smulv_optab->handlers[(int) (int) DFmode].insn_code =
    smul_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_muldf3;
  if (HAVE_mulsf3)
    smulv_optab->handlers[(int) (int) SFmode].insn_code =
    smul_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_mulsf3;
  smul_optab->handlers[(int) SImode].insn_code = CODE_FOR_mulsi3;
  if (HAVE_muldi3)
    smul_optab->handlers[(int) DImode].insn_code = CODE_FOR_muldi3;
  smul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_mulsidi3;
  umul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_umulsidi3;
  smul_highpart_optab->handlers[(int) SImode].insn_code = CODE_FOR_smulsi3_highpart;
  umul_highpart_optab->handlers[(int) SImode].insn_code = CODE_FOR_umulsi3_highpart;
  if (HAVE_divmodsi4)
    sdivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_divmodsi4;
  if (HAVE_divmoddi4)
    sdivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_divmoddi4;
  if (HAVE_udivmodsi4)
    udivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivmodsi4;
  if (HAVE_udivmoddi4)
    udivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivmoddi4;
  if (HAVE_divsi3)
    sdiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_divsi3;
  if (HAVE_divdi3)
    sdiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_divdi3;
  if (HAVE_modsi3)
    smod_optab->handlers[(int) SImode].insn_code = CODE_FOR_modsi3;
  if (HAVE_moddi3)
    smod_optab->handlers[(int) DImode].insn_code = CODE_FOR_moddi3;
  if (HAVE_udivsi3)
    udiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivsi3;
  if (HAVE_udivdi3)
    udiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivdi3;
  if (HAVE_umodsi3)
    umod_optab->handlers[(int) SImode].insn_code = CODE_FOR_umodsi3;
  if (HAVE_umoddi3)
    umod_optab->handlers[(int) DImode].insn_code = CODE_FOR_umoddi3;
  if (HAVE_negdi2)
    neg_optab->handlers[(int) DImode].insn_code = CODE_FOR_negdi2;
  and_optab->handlers[(int) SImode].insn_code = CODE_FOR_andsi3;
  if (HAVE_anddi3)
    and_optab->handlers[(int) DImode].insn_code = CODE_FOR_anddi3;
  ior_optab->handlers[(int) SImode].insn_code = CODE_FOR_iorsi3;
  if (HAVE_iordi3)
    ior_optab->handlers[(int) DImode].insn_code = CODE_FOR_iordi3;
  xor_optab->handlers[(int) SImode].insn_code = CODE_FOR_xorsi3;
  if (HAVE_xordi3)
    xor_optab->handlers[(int) DImode].insn_code = CODE_FOR_xordi3;
  if (HAVE_zero_extendsidi2)
    extendtab[(int) DImode][(int) SImode][1] = CODE_FOR_zero_extendsidi2;
  extendtab[(int) SImode][(int) HImode][1] = CODE_FOR_zero_extendhisi2;
  if (HAVE_zero_extendhidi2)
    extendtab[(int) DImode][(int) HImode][1] = CODE_FOR_zero_extendhidi2;
  extendtab[(int) HImode][(int) QImode][1] = CODE_FOR_zero_extendqihi2;
  extendtab[(int) SImode][(int) QImode][1] = CODE_FOR_zero_extendqisi2;
  if (HAVE_zero_extendqidi2)
    extendtab[(int) DImode][(int) QImode][1] = CODE_FOR_zero_extendqidi2;
  if (HAVE_extendsidi2)
    extendtab[(int) DImode][(int) SImode][0] = CODE_FOR_extendsidi2;
  if (HAVE_extendhidi2)
    extendtab[(int) DImode][(int) HImode][0] = CODE_FOR_extendhidi2;
  extendtab[(int) SImode][(int) HImode][0] = CODE_FOR_extendhisi2;
  extendtab[(int) HImode][(int) QImode][0] = CODE_FOR_extendqihi2;
  extendtab[(int) SImode][(int) QImode][0] = CODE_FOR_extendqisi2;
  if (HAVE_extendqidi2)
    extendtab[(int) DImode][(int) QImode][0] = CODE_FOR_extendqidi2;
  if (HAVE_fix_truncdfsi2)
    fixtrunctab[(int) DFmode][(int) SImode][0] = CODE_FOR_fix_truncdfsi2;
  if (HAVE_fix_truncsfsi2)
    fixtrunctab[(int) SFmode][(int) SImode][0] = CODE_FOR_fix_truncsfsi2;
  if (HAVE_fixuns_truncdfsi2)
    fixtrunctab[(int) DFmode][(int) SImode][1] = CODE_FOR_fixuns_truncdfsi2;
  if (HAVE_fixuns_truncdfdi2)
    fixtrunctab[(int) DFmode][(int) DImode][1] = CODE_FOR_fixuns_truncdfdi2;
  if (HAVE_fixuns_truncsfsi2)
    fixtrunctab[(int) SFmode][(int) SImode][1] = CODE_FOR_fixuns_truncsfsi2;
  if (HAVE_fixuns_truncsfdi2)
    fixtrunctab[(int) SFmode][(int) DImode][1] = CODE_FOR_fixuns_truncsfdi2;
  mov_optab->handlers[(int) DImode].insn_code = CODE_FOR_movdi;
  if (HAVE_reload_indi)
    reload_in_optab[(int) DImode] = CODE_FOR_reload_indi;
  if (HAVE_reload_outdi)
    reload_out_optab[(int) DImode] = CODE_FOR_reload_outdi;
  mov_optab->handlers[(int) SImode].insn_code = CODE_FOR_movsi;
  if (HAVE_reload_outsi)
    reload_out_optab[(int) SImode] = CODE_FOR_reload_outsi;
  if (HAVE_reload_insi)
    reload_in_optab[(int) SImode] = CODE_FOR_reload_insi;
  if (HAVE_reload_incc)
    reload_in_optab[(int) CCmode] = CODE_FOR_reload_incc;
  if (HAVE_reload_outcc)
    reload_out_optab[(int) CCmode] = CODE_FOR_reload_outcc;
  mov_optab->handlers[(int) HImode].insn_code = CODE_FOR_movhi;
  mov_optab->handlers[(int) QImode].insn_code = CODE_FOR_movqi;
  mov_optab->handlers[(int) SFmode].insn_code = CODE_FOR_movsf;
  mov_optab->handlers[(int) DFmode].insn_code = CODE_FOR_movdf;
  if (HAVE_movstrsi)
    movstr_optab[(int) SImode] = CODE_FOR_movstrsi;
  ashl_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashlsi3;
  if (HAVE_ashldi3)
    ashl_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashldi3;
  ashr_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashrsi3;
  if (HAVE_ashrdi3)
    ashr_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashrdi3;
  lshr_optab->handlers[(int) SImode].insn_code = CODE_FOR_lshrsi3;
  if (HAVE_lshrdi3)
    lshr_optab->handlers[(int) DImode].insn_code = CODE_FOR_lshrdi3;
  cmp_optab->handlers[(int) SImode].insn_code = CODE_FOR_cmpsi;
  tst_optab->handlers[(int) SImode].insn_code = CODE_FOR_tstsi;
  if (HAVE_cmpdi)
    cmp_optab->handlers[(int) DImode].insn_code = CODE_FOR_cmpdi;
  if (HAVE_tstdi)
    tst_optab->handlers[(int) DImode].insn_code = CODE_FOR_tstdi;
  if (HAVE_cmpdf)
    cmp_optab->handlers[(int) DFmode].insn_code = CODE_FOR_cmpdf;
  if (HAVE_cmpsf)
    cmp_optab->handlers[(int) SFmode].insn_code = CODE_FOR_cmpsf;
  bcc_gen_fctn[(int) UNORDERED] = gen_bunordered;
  bcc_gen_fctn[(int) ORDERED] = gen_bordered;
  bcc_gen_fctn[(int) UNLT] = gen_bunlt;
  bcc_gen_fctn[(int) UNGE] = gen_bunge;
  bcc_gen_fctn[(int) UNEQ] = gen_buneq;
  bcc_gen_fctn[(int) LTGT] = gen_bltgt;
  bcc_gen_fctn[(int) UNLE] = gen_bunle;
  bcc_gen_fctn[(int) UNGT] = gen_bungt;
  bcc_gen_fctn[(int) EQ] = gen_beq;
  bcc_gen_fctn[(int) NE] = gen_bne;
  bcc_gen_fctn[(int) GT] = gen_bgt;
  bcc_gen_fctn[(int) GE] = gen_bge;
  bcc_gen_fctn[(int) LT] = gen_blt;
  bcc_gen_fctn[(int) LE] = gen_ble;
  bcc_gen_fctn[(int) GTU] = gen_bgtu;
  bcc_gen_fctn[(int) GEU] = gen_bgeu;
  bcc_gen_fctn[(int) LTU] = gen_bltu;
  bcc_gen_fctn[(int) LEU] = gen_bleu;
  setcc_gen_code[(int) EQ] = CODE_FOR_seq;
  if (HAVE_sne)
    setcc_gen_code[(int) NE] = CODE_FOR_sne;
  setcc_gen_code[(int) GT] = CODE_FOR_sgt;
  setcc_gen_code[(int) GE] = CODE_FOR_sge;
  setcc_gen_code[(int) LT] = CODE_FOR_slt;
  setcc_gen_code[(int) LE] = CODE_FOR_sle;
  setcc_gen_code[(int) GTU] = CODE_FOR_sgtu;
  setcc_gen_code[(int) GEU] = CODE_FOR_sgeu;
  setcc_gen_code[(int) LTU] = CODE_FOR_sltu;
  setcc_gen_code[(int) LEU] = CODE_FOR_sleu;
  if (HAVE_movsicc)
    movcc_gen_code[(int) SImode] = CODE_FOR_movsicc;
  if (HAVE_movdicc)
    movcc_gen_code[(int) DImode] = CODE_FOR_movdicc;
  if (HAVE_movsfcc)
    movcc_gen_code[(int) SFmode] = CODE_FOR_movsfcc;
  if (HAVE_movdfcc)
    movcc_gen_code[(int) DFmode] = CODE_FOR_movdfcc;
}
