/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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
  if (HAVE_zero_extendhidi2)
    extendtab[(int) DImode][(int) HImode][1] = CODE_FOR_zero_extendhidi2;
  if (HAVE_zero_extendqidi2)
    extendtab[(int) DImode][(int) QImode][1] = CODE_FOR_zero_extendqidi2;
  if (HAVE_extendhidi2)
    extendtab[(int) DImode][(int) HImode][0] = CODE_FOR_extendhidi2;
  if (HAVE_extendqidi2)
    extendtab[(int) DImode][(int) QImode][0] = CODE_FOR_extendqidi2;
  extendtab[(int) SImode][(int) HImode][0] = CODE_FOR_extendhisi2;
  extendtab[(int) HImode][(int) QImode][0] = CODE_FOR_extendqihi2;
  extendtab[(int) SImode][(int) QImode][0] = CODE_FOR_extendqisi2;
  if (HAVE_floathisf2)
    floattab[(int) SFmode][(int) HImode][0] = CODE_FOR_floathisf2;
  if (HAVE_floathidf2)
    floattab[(int) DFmode][(int) HImode][0] = CODE_FOR_floathidf2;
  if (HAVE_floathixf2)
    floattab[(int) XFmode][(int) HImode][0] = CODE_FOR_floathixf2;
  if (HAVE_floathitf2)
    floattab[(int) TFmode][(int) HImode][0] = CODE_FOR_floathitf2;
  if (HAVE_floatsixf2)
    floattab[(int) XFmode][(int) SImode][0] = CODE_FOR_floatsixf2;
  if (HAVE_floatsitf2)
    floattab[(int) TFmode][(int) SImode][0] = CODE_FOR_floatsitf2;
  if (HAVE_floatdixf2)
    floattab[(int) XFmode][(int) DImode][0] = CODE_FOR_floatdixf2;
  if (HAVE_floatditf2)
    floattab[(int) TFmode][(int) DImode][0] = CODE_FOR_floatditf2;
  if (HAVE_divqi3)
    sdiv_optab->handlers[(int) QImode].insn_code = CODE_FOR_divqi3;
  if (HAVE_udivqi3)
    udiv_optab->handlers[(int) QImode].insn_code = CODE_FOR_udivqi3;
  if (HAVE_divmodhi4)
    sdivmod_optab->handlers[(int) HImode].insn_code = CODE_FOR_divmodhi4;
  if (HAVE_udivmoddi4)
    udivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivmoddi4;
  udivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivmodsi4;
  if (HAVE_sqrtxf2)
    sqrt_optab->handlers[(int) XFmode].insn_code = CODE_FOR_sqrtxf2;
  if (HAVE_sqrttf2)
    sqrt_optab->handlers[(int) TFmode].insn_code = CODE_FOR_sqrttf2;
  if (HAVE_sindf2)
    sin_optab->handlers[(int) DFmode].insn_code = CODE_FOR_sindf2;
  if (HAVE_sinsf2)
    sin_optab->handlers[(int) SFmode].insn_code = CODE_FOR_sinsf2;
  if (HAVE_sinxf2)
    sin_optab->handlers[(int) XFmode].insn_code = CODE_FOR_sinxf2;
  if (HAVE_sintf2)
    sin_optab->handlers[(int) TFmode].insn_code = CODE_FOR_sintf2;
  if (HAVE_cosdf2)
    cos_optab->handlers[(int) DFmode].insn_code = CODE_FOR_cosdf2;
  if (HAVE_cossf2)
    cos_optab->handlers[(int) SFmode].insn_code = CODE_FOR_cossf2;
  if (HAVE_cosxf2)
    cos_optab->handlers[(int) XFmode].insn_code = CODE_FOR_cosxf2;
  if (HAVE_costf2)
    cos_optab->handlers[(int) TFmode].insn_code = CODE_FOR_costf2;
  if (HAVE_addv4sf3)
    addv_optab->handlers[(int) (int) V4SFmode].insn_code =
    add_optab->handlers[(int) (int) V4SFmode].insn_code = CODE_FOR_addv4sf3;
  if (HAVE_subv4sf3)
    subv_optab->handlers[(int) (int) V4SFmode].insn_code =
    sub_optab->handlers[(int) (int) V4SFmode].insn_code = CODE_FOR_subv4sf3;
  if (HAVE_mulv4sf3)
    smulv_optab->handlers[(int) (int) V4SFmode].insn_code =
    smul_optab->handlers[(int) (int) V4SFmode].insn_code = CODE_FOR_mulv4sf3;
  if (HAVE_divv4sf3)
    sdiv_optab->handlers[(int) V4SFmode].insn_code = CODE_FOR_divv4sf3;
  if (HAVE_sqrtv4sf2)
    sqrt_optab->handlers[(int) V4SFmode].insn_code = CODE_FOR_sqrtv4sf2;
  if (HAVE_addv8qi3)
    add_optab->handlers[(int) V8QImode].insn_code = CODE_FOR_addv8qi3;
  if (HAVE_addv4hi3)
    add_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_addv4hi3;
  if (HAVE_addv2si3)
    add_optab->handlers[(int) V2SImode].insn_code = CODE_FOR_addv2si3;
  if (HAVE_subv8qi3)
    sub_optab->handlers[(int) V8QImode].insn_code = CODE_FOR_subv8qi3;
  if (HAVE_subv4hi3)
    sub_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_subv4hi3;
  if (HAVE_subv2si3)
    sub_optab->handlers[(int) V2SImode].insn_code = CODE_FOR_subv2si3;
  if (HAVE_mulv4hi3)
    smul_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_mulv4hi3;
  if (HAVE_smulv4hi3_highpart)
    smul_highpart_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_smulv4hi3_highpart;
  if (HAVE_umulv4hi3_highpart)
    umul_highpart_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_umulv4hi3_highpart;
  if (HAVE_umaxv8qi3)
    umax_optab->handlers[(int) V8QImode].insn_code = CODE_FOR_umaxv8qi3;
  if (HAVE_smaxv4hi3)
    smax_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_smaxv4hi3;
  if (HAVE_uminv8qi3)
    umin_optab->handlers[(int) V8QImode].insn_code = CODE_FOR_uminv8qi3;
  if (HAVE_sminv4hi3)
    smin_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_sminv4hi3;
  if (HAVE_ashrv4hi3)
    ashr_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_ashrv4hi3;
  if (HAVE_ashrv2si3)
    ashr_optab->handlers[(int) V2SImode].insn_code = CODE_FOR_ashrv2si3;
  if (HAVE_lshrv4hi3)
    lshr_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_lshrv4hi3;
  if (HAVE_lshrv2si3)
    lshr_optab->handlers[(int) V2SImode].insn_code = CODE_FOR_lshrv2si3;
  if (HAVE_ashlv4hi3)
    ashl_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_ashlv4hi3;
  if (HAVE_ashlv2si3)
    ashl_optab->handlers[(int) V2SImode].insn_code = CODE_FOR_ashlv2si3;
  if (HAVE_addv2sf3)
    addv_optab->handlers[(int) (int) V2SFmode].insn_code =
    add_optab->handlers[(int) (int) V2SFmode].insn_code = CODE_FOR_addv2sf3;
  if (HAVE_subv2sf3)
    subv_optab->handlers[(int) (int) V2SFmode].insn_code =
    sub_optab->handlers[(int) (int) V2SFmode].insn_code = CODE_FOR_subv2sf3;
  if (HAVE_mulv2sf3)
    smulv_optab->handlers[(int) (int) V2SFmode].insn_code =
    smul_optab->handlers[(int) (int) V2SFmode].insn_code = CODE_FOR_mulv2sf3;
  if (HAVE_addv2df3)
    addv_optab->handlers[(int) (int) V2DFmode].insn_code =
    add_optab->handlers[(int) (int) V2DFmode].insn_code = CODE_FOR_addv2df3;
  if (HAVE_subv2df3)
    subv_optab->handlers[(int) (int) V2DFmode].insn_code =
    sub_optab->handlers[(int) (int) V2DFmode].insn_code = CODE_FOR_subv2df3;
  if (HAVE_mulv2df3)
    smulv_optab->handlers[(int) (int) V2DFmode].insn_code =
    smul_optab->handlers[(int) (int) V2DFmode].insn_code = CODE_FOR_mulv2df3;
  if (HAVE_divv2df3)
    sdiv_optab->handlers[(int) V2DFmode].insn_code = CODE_FOR_divv2df3;
  if (HAVE_sqrtv2df2)
    sqrt_optab->handlers[(int) V2DFmode].insn_code = CODE_FOR_sqrtv2df2;
  if (HAVE_addv16qi3)
    add_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_addv16qi3;
  if (HAVE_addv8hi3)
    add_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_addv8hi3;
  if (HAVE_addv4si3)
    add_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_addv4si3;
  if (HAVE_addv2di3)
    add_optab->handlers[(int) V2DImode].insn_code = CODE_FOR_addv2di3;
  if (HAVE_subv16qi3)
    sub_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_subv16qi3;
  if (HAVE_subv8hi3)
    sub_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_subv8hi3;
  if (HAVE_subv4si3)
    sub_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_subv4si3;
  if (HAVE_subv2di3)
    sub_optab->handlers[(int) V2DImode].insn_code = CODE_FOR_subv2di3;
  if (HAVE_mulv8hi3)
    smul_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_mulv8hi3;
  if (HAVE_smulv8hi3_highpart)
    smul_highpart_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_smulv8hi3_highpart;
  if (HAVE_umulv8hi3_highpart)
    umul_highpart_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_umulv8hi3_highpart;
  if (HAVE_umaxv16qi3)
    umax_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_umaxv16qi3;
  if (HAVE_smaxv8hi3)
    smax_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_smaxv8hi3;
  if (HAVE_uminv16qi3)
    umin_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_uminv16qi3;
  if (HAVE_sminv8hi3)
    smin_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_sminv8hi3;
  if (HAVE_ashrv8hi3)
    ashr_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_ashrv8hi3;
  if (HAVE_ashrv4si3)
    ashr_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_ashrv4si3;
  if (HAVE_lshrv8hi3)
    lshr_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_lshrv8hi3;
  if (HAVE_lshrv4si3)
    lshr_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_lshrv4si3;
  if (HAVE_lshrv2di3)
    lshr_optab->handlers[(int) V2DImode].insn_code = CODE_FOR_lshrv2di3;
  if (HAVE_ashlv8hi3)
    ashl_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_ashlv8hi3;
  if (HAVE_ashlv4si3)
    ashl_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_ashlv4si3;
  if (HAVE_ashlv2di3)
    ashl_optab->handlers[(int) V2DImode].insn_code = CODE_FOR_ashlv2di3;
  cmp_optab->handlers[(int) DImode].insn_code = CODE_FOR_cmpdi;
  cmp_optab->handlers[(int) SImode].insn_code = CODE_FOR_cmpsi;
  cmp_optab->handlers[(int) HImode].insn_code = CODE_FOR_cmphi;
  if (HAVE_cmpqi)
    cmp_optab->handlers[(int) QImode].insn_code = CODE_FOR_cmpqi;
  if (HAVE_cmpxf)
    cmp_optab->handlers[(int) XFmode].insn_code = CODE_FOR_cmpxf;
  if (HAVE_cmptf)
    cmp_optab->handlers[(int) TFmode].insn_code = CODE_FOR_cmptf;
  if (HAVE_cmpdf)
    cmp_optab->handlers[(int) DFmode].insn_code = CODE_FOR_cmpdf;
  if (HAVE_cmpsf)
    cmp_optab->handlers[(int) SFmode].insn_code = CODE_FOR_cmpsf;
  mov_optab->handlers[(int) SImode].insn_code = CODE_FOR_movsi;
  mov_optab->handlers[(int) HImode].insn_code = CODE_FOR_movhi;
  if (HAVE_movstricthi)
    movstrict_optab->handlers[(int) HImode].insn_code = CODE_FOR_movstricthi;
  mov_optab->handlers[(int) QImode].insn_code = CODE_FOR_movqi;
  reload_out_optab[(int) QImode] = CODE_FOR_reload_outqi;
  if (HAVE_movstrictqi)
    movstrict_optab->handlers[(int) QImode].insn_code = CODE_FOR_movstrictqi;
  mov_optab->handlers[(int) DImode].insn_code = CODE_FOR_movdi;
  mov_optab->handlers[(int) SFmode].insn_code = CODE_FOR_movsf;
  mov_optab->handlers[(int) DFmode].insn_code = CODE_FOR_movdf;
  if (HAVE_movxf)
    mov_optab->handlers[(int) XFmode].insn_code = CODE_FOR_movxf;
  mov_optab->handlers[(int) TFmode].insn_code = CODE_FOR_movtf;
  extendtab[(int) SImode][(int) HImode][1] = CODE_FOR_zero_extendhisi2;
  extendtab[(int) HImode][(int) QImode][1] = CODE_FOR_zero_extendqihi2;
  extendtab[(int) SImode][(int) QImode][1] = CODE_FOR_zero_extendqisi2;
  extendtab[(int) DImode][(int) SImode][1] = CODE_FOR_zero_extendsidi2;
  extendtab[(int) DImode][(int) SImode][0] = CODE_FOR_extendsidi2;
  if (HAVE_extendsfdf2)
    extendtab[(int) DFmode][(int) SFmode][0] = CODE_FOR_extendsfdf2;
  if (HAVE_extendsfxf2)
    extendtab[(int) XFmode][(int) SFmode][0] = CODE_FOR_extendsfxf2;
  if (HAVE_extendsftf2)
    extendtab[(int) TFmode][(int) SFmode][0] = CODE_FOR_extendsftf2;
  if (HAVE_extenddfxf2)
    extendtab[(int) XFmode][(int) DFmode][0] = CODE_FOR_extenddfxf2;
  if (HAVE_extenddftf2)
    extendtab[(int) TFmode][(int) DFmode][0] = CODE_FOR_extenddftf2;
  if (HAVE_fix_truncxfdi2)
    fixtrunctab[(int) XFmode][(int) DImode][0] = CODE_FOR_fix_truncxfdi2;
  if (HAVE_fix_trunctfdi2)
    fixtrunctab[(int) TFmode][(int) DImode][0] = CODE_FOR_fix_trunctfdi2;
  if (HAVE_fix_truncdfdi2)
    fixtrunctab[(int) DFmode][(int) DImode][0] = CODE_FOR_fix_truncdfdi2;
  if (HAVE_fix_truncsfdi2)
    fixtrunctab[(int) SFmode][(int) DImode][0] = CODE_FOR_fix_truncsfdi2;
  if (HAVE_fix_truncxfsi2)
    fixtrunctab[(int) XFmode][(int) SImode][0] = CODE_FOR_fix_truncxfsi2;
  if (HAVE_fix_trunctfsi2)
    fixtrunctab[(int) TFmode][(int) SImode][0] = CODE_FOR_fix_trunctfsi2;
  if (HAVE_fix_truncdfsi2)
    fixtrunctab[(int) DFmode][(int) SImode][0] = CODE_FOR_fix_truncdfsi2;
  if (HAVE_fix_truncsfsi2)
    fixtrunctab[(int) SFmode][(int) SImode][0] = CODE_FOR_fix_truncsfsi2;
  if (HAVE_fix_truncxfhi2)
    fixtrunctab[(int) XFmode][(int) HImode][0] = CODE_FOR_fix_truncxfhi2;
  if (HAVE_fix_trunctfhi2)
    fixtrunctab[(int) TFmode][(int) HImode][0] = CODE_FOR_fix_trunctfhi2;
  if (HAVE_fix_truncdfhi2)
    fixtrunctab[(int) DFmode][(int) HImode][0] = CODE_FOR_fix_truncdfhi2;
  if (HAVE_fix_truncsfhi2)
    fixtrunctab[(int) SFmode][(int) HImode][0] = CODE_FOR_fix_truncsfhi2;
  if (HAVE_floatsisf2)
    floattab[(int) SFmode][(int) SImode][0] = CODE_FOR_floatsisf2;
  if (HAVE_floatdisf2)
    floattab[(int) SFmode][(int) DImode][0] = CODE_FOR_floatdisf2;
  if (HAVE_floatsidf2)
    floattab[(int) DFmode][(int) SImode][0] = CODE_FOR_floatsidf2;
  if (HAVE_floatdidf2)
    floattab[(int) DFmode][(int) DImode][0] = CODE_FOR_floatdidf2;
  add_optab->handlers[(int) DImode].insn_code = CODE_FOR_adddi3;
  add_optab->handlers[(int) SImode].insn_code = CODE_FOR_addsi3;
  if (HAVE_addhi3)
    add_optab->handlers[(int) HImode].insn_code = CODE_FOR_addhi3;
  if (HAVE_addqi3)
    add_optab->handlers[(int) QImode].insn_code = CODE_FOR_addqi3;
  if (HAVE_addxf3)
    addv_optab->handlers[(int) (int) XFmode].insn_code =
    add_optab->handlers[(int) (int) XFmode].insn_code = CODE_FOR_addxf3;
  if (HAVE_addtf3)
    addv_optab->handlers[(int) (int) TFmode].insn_code =
    add_optab->handlers[(int) (int) TFmode].insn_code = CODE_FOR_addtf3;
  if (HAVE_adddf3)
    addv_optab->handlers[(int) (int) DFmode].insn_code =
    add_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_adddf3;
  if (HAVE_addsf3)
    addv_optab->handlers[(int) (int) SFmode].insn_code =
    add_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_addsf3;
  sub_optab->handlers[(int) DImode].insn_code = CODE_FOR_subdi3;
  sub_optab->handlers[(int) SImode].insn_code = CODE_FOR_subsi3;
  if (HAVE_subhi3)
    sub_optab->handlers[(int) HImode].insn_code = CODE_FOR_subhi3;
  if (HAVE_subqi3)
    sub_optab->handlers[(int) QImode].insn_code = CODE_FOR_subqi3;
  if (HAVE_subxf3)
    subv_optab->handlers[(int) (int) XFmode].insn_code =
    sub_optab->handlers[(int) (int) XFmode].insn_code = CODE_FOR_subxf3;
  if (HAVE_subtf3)
    subv_optab->handlers[(int) (int) TFmode].insn_code =
    sub_optab->handlers[(int) (int) TFmode].insn_code = CODE_FOR_subtf3;
  if (HAVE_subdf3)
    subv_optab->handlers[(int) (int) DFmode].insn_code =
    sub_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_subdf3;
  if (HAVE_subsf3)
    subv_optab->handlers[(int) (int) SFmode].insn_code =
    sub_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_subsf3;
  if (HAVE_muldi3)
    smul_optab->handlers[(int) DImode].insn_code = CODE_FOR_muldi3;
  smul_optab->handlers[(int) SImode].insn_code = CODE_FOR_mulsi3;
  if (HAVE_mulhi3)
    smul_optab->handlers[(int) HImode].insn_code = CODE_FOR_mulhi3;
  if (HAVE_mulqi3)
    smul_optab->handlers[(int) QImode].insn_code = CODE_FOR_mulqi3;
  if (HAVE_umulqihi3)
    umul_widen_optab->handlers[(int) HImode].insn_code = CODE_FOR_umulqihi3;
  if (HAVE_mulqihi3)
    smul_widen_optab->handlers[(int) HImode].insn_code = CODE_FOR_mulqihi3;
  if (HAVE_umulditi3)
    umul_widen_optab->handlers[(int) TImode].insn_code = CODE_FOR_umulditi3;
  if (HAVE_umulsidi3)
    umul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_umulsidi3;
  if (HAVE_mulditi3)
    smul_widen_optab->handlers[(int) TImode].insn_code = CODE_FOR_mulditi3;
  if (HAVE_mulsidi3)
    smul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_mulsidi3;
  if (HAVE_umuldi3_highpart)
    umul_highpart_optab->handlers[(int) DImode].insn_code = CODE_FOR_umuldi3_highpart;
  umul_highpart_optab->handlers[(int) SImode].insn_code = CODE_FOR_umulsi3_highpart;
  if (HAVE_smuldi3_highpart)
    smul_highpart_optab->handlers[(int) DImode].insn_code = CODE_FOR_smuldi3_highpart;
  smul_highpart_optab->handlers[(int) SImode].insn_code = CODE_FOR_smulsi3_highpart;
  if (HAVE_mulxf3)
    smulv_optab->handlers[(int) (int) XFmode].insn_code =
    smul_optab->handlers[(int) (int) XFmode].insn_code = CODE_FOR_mulxf3;
  if (HAVE_multf3)
    smulv_optab->handlers[(int) (int) TFmode].insn_code =
    smul_optab->handlers[(int) (int) TFmode].insn_code = CODE_FOR_multf3;
  if (HAVE_muldf3)
    smulv_optab->handlers[(int) (int) DFmode].insn_code =
    smul_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_muldf3;
  if (HAVE_mulsf3)
    smulv_optab->handlers[(int) (int) SFmode].insn_code =
    smul_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_mulsf3;
  if (HAVE_divxf3)
    sdiv_optab->handlers[(int) XFmode].insn_code = CODE_FOR_divxf3;
  if (HAVE_divtf3)
    sdiv_optab->handlers[(int) TFmode].insn_code = CODE_FOR_divtf3;
  if (HAVE_divdf3)
    sdiv_optab->handlers[(int) DFmode].insn_code = CODE_FOR_divdf3;
  if (HAVE_divsf3)
    sdiv_optab->handlers[(int) SFmode].insn_code = CODE_FOR_divsf3;
  if (HAVE_divmoddi4)
    sdivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_divmoddi4;
  sdivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_divmodsi4;
  if (HAVE_udivmodhi4)
    udivmod_optab->handlers[(int) HImode].insn_code = CODE_FOR_udivmodhi4;
  if (HAVE_anddi3)
    and_optab->handlers[(int) DImode].insn_code = CODE_FOR_anddi3;
  and_optab->handlers[(int) SImode].insn_code = CODE_FOR_andsi3;
  if (HAVE_andhi3)
    and_optab->handlers[(int) HImode].insn_code = CODE_FOR_andhi3;
  if (HAVE_andqi3)
    and_optab->handlers[(int) QImode].insn_code = CODE_FOR_andqi3;
  if (HAVE_iordi3)
    ior_optab->handlers[(int) DImode].insn_code = CODE_FOR_iordi3;
  ior_optab->handlers[(int) SImode].insn_code = CODE_FOR_iorsi3;
  if (HAVE_iorhi3)
    ior_optab->handlers[(int) HImode].insn_code = CODE_FOR_iorhi3;
  if (HAVE_iorqi3)
    ior_optab->handlers[(int) QImode].insn_code = CODE_FOR_iorqi3;
  if (HAVE_xordi3)
    xor_optab->handlers[(int) DImode].insn_code = CODE_FOR_xordi3;
  xor_optab->handlers[(int) SImode].insn_code = CODE_FOR_xorsi3;
  if (HAVE_xorhi3)
    xor_optab->handlers[(int) HImode].insn_code = CODE_FOR_xorhi3;
  if (HAVE_xorqi3)
    xor_optab->handlers[(int) QImode].insn_code = CODE_FOR_xorqi3;
  neg_optab->handlers[(int) DImode].insn_code = CODE_FOR_negdi2;
  neg_optab->handlers[(int) SImode].insn_code = CODE_FOR_negsi2;
  if (HAVE_neghi2)
    neg_optab->handlers[(int) HImode].insn_code = CODE_FOR_neghi2;
  if (HAVE_negqi2)
    neg_optab->handlers[(int) QImode].insn_code = CODE_FOR_negqi2;
  if (HAVE_negsf2)
    negv_optab->handlers[(int) (int) SFmode].insn_code =
    neg_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_negsf2;
  if (HAVE_negdf2)
    negv_optab->handlers[(int) (int) DFmode].insn_code =
    neg_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_negdf2;
  if (HAVE_negxf2)
    negv_optab->handlers[(int) (int) XFmode].insn_code =
    neg_optab->handlers[(int) (int) XFmode].insn_code = CODE_FOR_negxf2;
  if (HAVE_negtf2)
    negv_optab->handlers[(int) (int) TFmode].insn_code =
    neg_optab->handlers[(int) (int) TFmode].insn_code = CODE_FOR_negtf2;
  if (HAVE_abssf2)
    absv_optab->handlers[(int) (int) SFmode].insn_code =
    abs_optab->handlers[(int) (int) SFmode].insn_code = CODE_FOR_abssf2;
  if (HAVE_absdf2)
    absv_optab->handlers[(int) (int) DFmode].insn_code =
    abs_optab->handlers[(int) (int) DFmode].insn_code = CODE_FOR_absdf2;
  if (HAVE_absxf2)
    absv_optab->handlers[(int) (int) XFmode].insn_code =
    abs_optab->handlers[(int) (int) XFmode].insn_code = CODE_FOR_absxf2;
  if (HAVE_abstf2)
    absv_optab->handlers[(int) (int) TFmode].insn_code =
    abs_optab->handlers[(int) (int) TFmode].insn_code = CODE_FOR_abstf2;
  if (HAVE_one_cmpldi2)
    one_cmpl_optab->handlers[(int) DImode].insn_code = CODE_FOR_one_cmpldi2;
  one_cmpl_optab->handlers[(int) SImode].insn_code = CODE_FOR_one_cmplsi2;
  if (HAVE_one_cmplhi2)
    one_cmpl_optab->handlers[(int) HImode].insn_code = CODE_FOR_one_cmplhi2;
  if (HAVE_one_cmplqi2)
    one_cmpl_optab->handlers[(int) QImode].insn_code = CODE_FOR_one_cmplqi2;
  ashl_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashldi3;
  ashl_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashlsi3;
  if (HAVE_ashlhi3)
    ashl_optab->handlers[(int) HImode].insn_code = CODE_FOR_ashlhi3;
  if (HAVE_ashlqi3)
    ashl_optab->handlers[(int) QImode].insn_code = CODE_FOR_ashlqi3;
  ashr_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashrdi3;
  ashr_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashrsi3;
  if (HAVE_ashrhi3)
    ashr_optab->handlers[(int) HImode].insn_code = CODE_FOR_ashrhi3;
  if (HAVE_ashrqi3)
    ashr_optab->handlers[(int) QImode].insn_code = CODE_FOR_ashrqi3;
  lshr_optab->handlers[(int) DImode].insn_code = CODE_FOR_lshrdi3;
  lshr_optab->handlers[(int) SImode].insn_code = CODE_FOR_lshrsi3;
  if (HAVE_lshrhi3)
    lshr_optab->handlers[(int) HImode].insn_code = CODE_FOR_lshrhi3;
  if (HAVE_lshrqi3)
    lshr_optab->handlers[(int) QImode].insn_code = CODE_FOR_lshrqi3;
  if (HAVE_rotldi3)
    rotl_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotldi3;
  rotl_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotlsi3;
  if (HAVE_rotlhi3)
    rotl_optab->handlers[(int) HImode].insn_code = CODE_FOR_rotlhi3;
  if (HAVE_rotlqi3)
    rotl_optab->handlers[(int) QImode].insn_code = CODE_FOR_rotlqi3;
  if (HAVE_rotrdi3)
    rotr_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotrdi3;
  rotr_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotrsi3;
  if (HAVE_rotrhi3)
    rotr_optab->handlers[(int) HImode].insn_code = CODE_FOR_rotrhi3;
  if (HAVE_rotrqi3)
    rotr_optab->handlers[(int) QImode].insn_code = CODE_FOR_rotrqi3;
  setcc_gen_code[(int) EQ] = CODE_FOR_seq;
  setcc_gen_code[(int) NE] = CODE_FOR_sne;
  setcc_gen_code[(int) GT] = CODE_FOR_sgt;
  setcc_gen_code[(int) GTU] = CODE_FOR_sgtu;
  setcc_gen_code[(int) LT] = CODE_FOR_slt;
  setcc_gen_code[(int) LTU] = CODE_FOR_sltu;
  setcc_gen_code[(int) GE] = CODE_FOR_sge;
  setcc_gen_code[(int) GEU] = CODE_FOR_sgeu;
  setcc_gen_code[(int) LE] = CODE_FOR_sle;
  setcc_gen_code[(int) LEU] = CODE_FOR_sleu;
  if (HAVE_sunordered)
    setcc_gen_code[(int) UNORDERED] = CODE_FOR_sunordered;
  if (HAVE_sordered)
    setcc_gen_code[(int) ORDERED] = CODE_FOR_sordered;
  if (HAVE_suneq)
    setcc_gen_code[(int) UNEQ] = CODE_FOR_suneq;
  if (HAVE_sunge)
    setcc_gen_code[(int) UNGE] = CODE_FOR_sunge;
  if (HAVE_sungt)
    setcc_gen_code[(int) UNGT] = CODE_FOR_sungt;
  if (HAVE_sunle)
    setcc_gen_code[(int) UNLE] = CODE_FOR_sunle;
  if (HAVE_sunlt)
    setcc_gen_code[(int) UNLT] = CODE_FOR_sunlt;
  if (HAVE_sltgt)
    setcc_gen_code[(int) LTGT] = CODE_FOR_sltgt;
  bcc_gen_fctn[(int) EQ] = gen_beq;
  bcc_gen_fctn[(int) NE] = gen_bne;
  bcc_gen_fctn[(int) GT] = gen_bgt;
  bcc_gen_fctn[(int) GTU] = gen_bgtu;
  bcc_gen_fctn[(int) LT] = gen_blt;
  bcc_gen_fctn[(int) LTU] = gen_bltu;
  bcc_gen_fctn[(int) GE] = gen_bge;
  bcc_gen_fctn[(int) GEU] = gen_bgeu;
  bcc_gen_fctn[(int) LE] = gen_ble;
  bcc_gen_fctn[(int) LEU] = gen_bleu;
  if (HAVE_bunordered)
    bcc_gen_fctn[(int) UNORDERED] = gen_bunordered;
  if (HAVE_bordered)
    bcc_gen_fctn[(int) ORDERED] = gen_bordered;
  if (HAVE_buneq)
    bcc_gen_fctn[(int) UNEQ] = gen_buneq;
  if (HAVE_bunge)
    bcc_gen_fctn[(int) UNGE] = gen_bunge;
  if (HAVE_bungt)
    bcc_gen_fctn[(int) UNGT] = gen_bungt;
  if (HAVE_bunle)
    bcc_gen_fctn[(int) UNLE] = gen_bunle;
  if (HAVE_bunlt)
    bcc_gen_fctn[(int) UNLT] = gen_bunlt;
  if (HAVE_bltgt)
    bcc_gen_fctn[(int) LTGT] = gen_bltgt;
  ffs_optab->handlers[(int) SImode].insn_code = CODE_FOR_ffssi2;
  if (HAVE_sqrtsf2)
    sqrt_optab->handlers[(int) SFmode].insn_code = CODE_FOR_sqrtsf2;
  if (HAVE_sqrtdf2)
    sqrt_optab->handlers[(int) DFmode].insn_code = CODE_FOR_sqrtdf2;
  movstr_optab[(int) SImode] = CODE_FOR_movstrsi;
  if (HAVE_movstrdi)
    movstr_optab[(int) DImode] = CODE_FOR_movstrdi;
  clrstr_optab[(int) SImode] = CODE_FOR_clrstrsi;
  if (HAVE_clrstrdi)
    clrstr_optab[(int) DImode] = CODE_FOR_clrstrdi;
  strlen_optab->handlers[(int) SImode].insn_code = CODE_FOR_strlensi;
  strlen_optab->handlers[(int) DImode].insn_code = CODE_FOR_strlendi;
  if (HAVE_movdicc)
    movcc_gen_code[(int) DImode] = CODE_FOR_movdicc;
  movcc_gen_code[(int) SImode] = CODE_FOR_movsicc;
  if (HAVE_movhicc)
    movcc_gen_code[(int) HImode] = CODE_FOR_movhicc;
  if (HAVE_movsfcc)
    movcc_gen_code[(int) SFmode] = CODE_FOR_movsfcc;
  if (HAVE_movdfcc)
    movcc_gen_code[(int) DFmode] = CODE_FOR_movdfcc;
  if (HAVE_movxfcc)
    movcc_gen_code[(int) XFmode] = CODE_FOR_movxfcc;
  if (HAVE_movtfcc)
    movcc_gen_code[(int) TFmode] = CODE_FOR_movtfcc;
  if (HAVE_minsf3)
    smin_optab->handlers[(int) SFmode].insn_code = CODE_FOR_minsf3;
  if (HAVE_mindf3)
    smin_optab->handlers[(int) DFmode].insn_code = CODE_FOR_mindf3;
  if (HAVE_maxsf3)
    smax_optab->handlers[(int) SFmode].insn_code = CODE_FOR_maxsf3;
  if (HAVE_maxdf3)
    smax_optab->handlers[(int) DFmode].insn_code = CODE_FOR_maxdf3;
  if (HAVE_movti)
    mov_optab->handlers[(int) TImode].insn_code = CODE_FOR_movti;
  if (HAVE_movv2df)
    mov_optab->handlers[(int) V2DFmode].insn_code = CODE_FOR_movv2df;
  if (HAVE_movv8hi)
    mov_optab->handlers[(int) V8HImode].insn_code = CODE_FOR_movv8hi;
  if (HAVE_movv16qi)
    mov_optab->handlers[(int) V16QImode].insn_code = CODE_FOR_movv16qi;
  if (HAVE_movv4sf)
    mov_optab->handlers[(int) V4SFmode].insn_code = CODE_FOR_movv4sf;
  if (HAVE_movv4si)
    mov_optab->handlers[(int) V4SImode].insn_code = CODE_FOR_movv4si;
  if (HAVE_movv2di)
    mov_optab->handlers[(int) V2DImode].insn_code = CODE_FOR_movv2di;
  if (HAVE_movv2si)
    mov_optab->handlers[(int) V2SImode].insn_code = CODE_FOR_movv2si;
  if (HAVE_movv4hi)
    mov_optab->handlers[(int) V4HImode].insn_code = CODE_FOR_movv4hi;
  if (HAVE_movv8qi)
    mov_optab->handlers[(int) V8QImode].insn_code = CODE_FOR_movv8qi;
  if (HAVE_movv2sf)
    mov_optab->handlers[(int) V2SFmode].insn_code = CODE_FOR_movv2sf;
}
