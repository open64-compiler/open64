/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

//
// Group TOPS with similar Printing format together. 
/////////////////////////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. Binary Operators
//   2. Jump, Branch and Trap operators
//   3. Multiply/Divide operators
//   4. Load/Store machine operators
//   5. FPU arithmetic/convert operators
//   6. FPU branch operators
//   7. FPU comparison operators
//   8. Coprocessor1 (FP) data movement operations
//   9. MIPS IV only arithmetic/cond_move FPU operations
//  10. Compiler IR and fictional operators
//
// Within each Print_Type instructions are listed in alphabetical order and
// as shown in the ISA manual
/////////////////////////////////////
//
//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/isa/ia64/isa_print.cxx,v $

#include <stddef.h>
#include <string.h>
#include "topcode.h"
#include "isa_print_gen.h"

// Multiple topcodes map to the same assembly name. To disambiguate the 
// topcodes, we append a suffix to the basename. By convention, the 
// suffix starts with an underscore. To get the assembly name we strip off
// the suffix.
static const char *asmname(TOP topcode)
{
  int c;
  int i;
  int j;
  const char *name = TOP_Name(topcode);
  char buf[100];

  for (i = 0; ; ++i) {
    c = name[i];
    if (c == '.' || c == '\0') return name;
    if (c == '_') break;
    buf[i] = c;
  }

  j = i;
  do {
    c = name[++j];
  } while (c != '.' && c != '\0');

  do {
    c = name[j++];
    buf[i++] = c;
  } while (c);
      
  return strdup(buf);
}

main()
{
  ISA_Print_Begin("ia64");

  Set_AsmName_Func(asmname);

  Define_Macro("END_GROUP", ";;");		// end-of-group marker
  Define_Macro("PREDICATE", "(%s)");		// predicate operand format
  Define_Macro("BEGIN_BUNDLE", "{ %s");		// bundle introducer
  Define_Macro("END_BUNDLE", "}");		// bundle terminator

/* ===== %5s %s ===== */
  ISA_PRINT_TYPE print_0;
  print_0 = ISA_Print_Type_Create("print_0", "%5s %s");
  Operand(0);		// qp
  Name();
  Instruction_Print_Group(print_0,
	TOP_fwb,	TOP_invala,	TOP_mf,
	TOP_mf_a,	TOP_srlz_d,	TOP_srlz_i,
	TOP_sync_i,
	TOP_UNDEFINED);

/* =====       %s ===== */
  ISA_PRINT_TYPE print_1;
  print_1 = ISA_Print_Type_Create("print_1", "      %s");
  Name();
  Instruction_Print_Group(print_1,
	TOP_bsw_0,	TOP_bsw_1,	TOP_clrrrb,
	TOP_clrrrb_pr,	TOP_cover,	TOP_epc,
	TOP_flushrs,	TOP_loadrs,	TOP_rfi,
	TOP_UNDEFINED);

/* ===== %5s %s%s ===== */
  ISA_PRINT_TYPE print_2;
  print_2 = ISA_Print_Type_Create("print_2", "%5s %s%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sf
  Instruction_Print_Group(print_2,
	TOP_fclrf,
	TOP_UNDEFINED);

/* ===== %5s %s %s ===== */
  ISA_PRINT_TYPE print_3;
  print_3 = ISA_Print_Type_Create("print_3", "%5s %s %s");
  Operand(0);		// qp
  Name();
  Operand(1);		// imm
  Instruction_Print_Group(print_3,
	TOP_break_b,	TOP_break_f,	TOP_break_i,
	TOP_break_m,	TOP_break_x,	TOP_fc,
	TOP_invala_e,	TOP_invala_f_e,	TOP_itc_d,
	TOP_itc_i,	TOP_nop_b,	TOP_nop_f,
	TOP_nop_i,	TOP_nop_m,	TOP_nop_x,
	TOP_ptc_e,	TOP_rsm,	TOP_rum,
	TOP_ssm,	TOP_sum,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s ===== */
  ISA_PRINT_TYPE print_4;
  print_4 = ISA_Print_Type_Create("print_4", "%5s %s%s %s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sf
  Operand(2);		// imm
  Instruction_Print_Group(print_4,
	TOP_fchkf,
	TOP_UNDEFINED);

/* ===== %5s %s %s,%s ===== */
  ISA_PRINT_TYPE print_5;
  print_5 = ISA_Print_Type_Create("print_5", "%5s %s %s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r2
  Operand(2);		// imm
  Instruction_Print_Group(print_5,
	TOP_chk_s_i,	TOP_chk_s_m,	TOP_chk_f_s,
	TOP_probe_r_fault,	TOP_probe_rw_fault,	TOP_probe_w_fault,
	TOP_ptc_g,	TOP_ptc_ga,	TOP_ptc_l,
	TOP_ptr_d,	TOP_ptr_i,
	TOP_UNDEFINED);

/* ===== %5s %s %s=%s ===== */
  ISA_PRINT_TYPE print_6;
  print_6 = ISA_Print_Type_Create("print_6", "%5s %s %s=%s");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_6,
	TOP_czx1_l,	TOP_czx1_r,	TOP_czx2_l,
	TOP_czx2_r,	TOP_fabs,	TOP_fcvt_xf,
	TOP_fneg,	TOP_fnegabs,	TOP_fpabs,
	TOP_fpneg,	TOP_fpnegabs,	TOP_getf_d,
	TOP_getf_exp,	TOP_getf_s,	TOP_getf_sig,
	TOP_mov,	TOP_mov_f,	TOP_mov_f_ar_i,
	TOP_mov_f_ar_m,	TOP_mov_f_br,	TOP_mov_f_cr,
	TOP_mov_i,	TOP_mov_t_ar_i_i,	TOP_mov_t_ar_i_m,
	TOP_mov_t_ar_r_i,	TOP_mov_t_ar_r_m,	TOP_mov_t_br,
	TOP_mov_t_cr,	TOP_movl,	TOP_popcnt,
	TOP_setf_d,	TOP_setf_exp,	TOP_setf_s,
	TOP_setf_sig,	TOP_sxt1,	TOP_sxt2,
	TOP_sxt4,	TOP_tak,	TOP_thash,
	TOP_tpa,	TOP_ttag,	TOP_zxt1,
	TOP_zxt2,	TOP_zxt4,
	TOP_UNDEFINED);

/* ===== %5s %s %s=ip ===== */
  ISA_PRINT_TYPE print_7;
  print_7 = ISA_Print_Type_Create("print_7", "%5s %s %s=ip");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Instruction_Print_Group(print_7,
	TOP_mov_f_ip,
	TOP_UNDEFINED);

/* ===== %5s %s %s=pr ===== */
  ISA_PRINT_TYPE print_8;
  print_8 = ISA_Print_Type_Create("print_8", "%5s %s %s=pr");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Instruction_Print_Group(print_8,
	TOP_mov_f_pr,
	TOP_UNDEFINED);

/* ===== %5s %s %s=psr ===== */
  ISA_PRINT_TYPE print_9;
  print_9 = ISA_Print_Type_Create("print_9", "%5s %s %s=psr");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Instruction_Print_Group(print_9,
	TOP_mov_f_psr,
	TOP_UNDEFINED);

/* ===== %5s %s%s [%s] ===== */
  ISA_PRINT_TYPE print_10;
  print_10 = ISA_Print_Type_Create("print_10", "%5s %s%s [%s]");
  Operand(0);		// qp
  Name();
  Operand(1);		// lfhint
  Operand(2);		// r3
  Instruction_Print_Group(print_10,
	TOP_lfetch,	TOP_lfetch_excl,	TOP_lfetch_fault,
	TOP_lfetch_fault_excl,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s,%s ===== */
  ISA_PRINT_TYPE print_11;
  print_11 = ISA_Print_Type_Create("print_11", "%5s %s%s %s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// aclr
  Operand(2);		// r1
  Operand(3);		// imm
  Instruction_Print_Group(print_11,
	TOP_chk_a,	TOP_chk_f_a,	TOP_fsetc,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s=%s ===== */
  ISA_PRINT_TYPE print_12;
  print_12 = ISA_Print_Type_Create("print_12", "%5s %s%s %s=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sf
  Result(0);		// f1
  Operand(2);		// f2
  Instruction_Print_Group(print_12,
	TOP_fcvt_fx,	TOP_fcvt_fx_trunc,	TOP_fcvt_fxu,
	TOP_fcvt_fxu_trunc,	TOP_fcvt_xuf,	TOP_fcvt_xuf_d,
	TOP_fcvt_xuf_s,	TOP_fnorm,	TOP_fnorm_d,
	TOP_fnorm_s,	TOP_fpcvt_fx,	TOP_fpcvt_fx_trunc,
	TOP_fpcvt_fxu,	TOP_fpcvt_fxu_trunc,
	TOP_UNDEFINED);

/* =====       %s%s%s %s ===== */
  ISA_PRINT_TYPE print_13;
  print_13 = ISA_Print_Type_Create("print_13", "      %s%s%s %s");
  Name();
  Operand(0);		// ph
  Operand(1);		// dh
  Operand(2);		// imm
  Instruction_Print_Group(print_13,
	TOP_br,	TOP_br_r,	TOP_brl,
	TOP_UNDEFINED);

/* ===== %5s %s %s,%s=%s ===== */
  ISA_PRINT_TYPE print_14;
  print_14 = ISA_Print_Type_Create("print_14", "%5s %s %s,%s=%s");
  Operand(0);		// qp
  Name();
  Result(0);		// p1
  Result(1);		// p2
  Operand(1);		// r3
  Instruction_Print_Group(print_14,
	TOP_tnat_nz,	TOP_tnat_nz_and,	TOP_tnat_nz_or,
	TOP_tnat_nz_or_andcm,	TOP_tnat_nz_unc,	TOP_tnat_z,
	TOP_tnat_z_and,	TOP_tnat_z_or,	TOP_tnat_z_or_andcm,
	TOP_tnat_z_unc,
	TOP_UNDEFINED);

/* ===== %5s %s %s=%s,%s ===== */
  ISA_PRINT_TYPE print_15;
  print_15 = ISA_Print_Type_Create("print_15", "%5s %s %s=%s,%s");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r2
  Operand(2);		// r3
  Instruction_Print_Group(print_15,
	TOP_add,	TOP_addl,	TOP_addp4,
	TOP_addp4_i,	TOP_adds,	TOP_and,
	TOP_and_i,	TOP_andcm,	TOP_andcm_i,
	TOP_fand,	TOP_fandcm,	TOP_fmerge_ns,
	TOP_fmerge_s,	TOP_fmerge_se,	TOP_fmix_l,
	TOP_fmix_lr,	TOP_fmix_r,	TOP_for,
	TOP_fpack,	TOP_fpmerge_ns,	TOP_fpmerge_s,
	TOP_fpmerge_se,	TOP_fswap,	TOP_fswap_nl,
	TOP_fswap_nr,	TOP_fsxt_l,	TOP_fsxt_r,
	TOP_fxor,	TOP_mix1_l,	TOP_mix1_r,
	TOP_mix2_l,	TOP_mix2_r,	TOP_mix4_l,
	TOP_mix4_r,	TOP_mux1,	TOP_mux2,
	TOP_or,	TOP_or_i,	TOP_pack2_sss,
	TOP_pack2_uss,	TOP_pack4_sss,	TOP_padd1,
	TOP_padd1_sss,	TOP_padd1_uus,	TOP_padd1_uuu,
	TOP_padd2,	TOP_padd2_sss,	TOP_padd2_uus,
	TOP_padd2_uuu,	TOP_padd4,	TOP_pavg1,
	TOP_pavg1_raz,	TOP_pavg2,	TOP_pavg2_raz,
	TOP_pavgsub1,	TOP_pavgsub2,	TOP_pcmp1_eq,
	TOP_pcmp1_gt,	TOP_pcmp2_eq,	TOP_pcmp2_gt,
	TOP_pcmp4_eq,	TOP_pcmp4_gt,	TOP_pmax1_u,
	TOP_pmax2,	TOP_pmin1_u,	TOP_pmin2,
	TOP_pmpy2_l,	TOP_pmpy2_r,	TOP_probe_r,
	TOP_probe_w,	TOP_probe_i_r,	TOP_probe_i_w,
	TOP_psad1,	TOP_pshl2,	TOP_pshl2_i,
	TOP_pshl4,	TOP_pshl4_i,	TOP_pshr2,
	TOP_pshr2_u,	TOP_pshr2_i,	TOP_pshr2_i_u,
	TOP_pshr4,	TOP_pshr4_u,	TOP_pshr4_i,
	TOP_pshr4_i_u,	TOP_psub1,	TOP_psub1_sss,
	TOP_psub1_uus,	TOP_psub1_uuu,	TOP_psub2,
	TOP_psub2_sss,	TOP_psub2_uus,	TOP_psub2_uuu,
	TOP_psub4,	TOP_shl,	TOP_shl_i,
	TOP_shr,	TOP_shr_u,	TOP_shr_i,
	TOP_shr_i_u,	TOP_sub,	TOP_sub_i,
	TOP_unpack1_h,	TOP_unpack1_l,	TOP_unpack2_h,
	TOP_unpack2_l,	TOP_unpack4_h,	TOP_unpack4_l,
	TOP_xmpy_h,	TOP_xmpy_hu,	TOP_xmpy_l,
	TOP_xmpy_lu,	TOP_xor,	TOP_xor_i,
	TOP_UNDEFINED);

/* ===== %5s %s pr=%s,%s ===== */
  ISA_PRINT_TYPE print_16;
  print_16 = ISA_Print_Type_Create("print_16", "%5s %s pr=%s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r2
  Operand(2);		// imm
  Instruction_Print_Group(print_16,
	TOP_mov_t_pr,
	TOP_UNDEFINED);

/* ===== %5s %s psr.l=%s ===== */
  ISA_PRINT_TYPE print_17;
  print_17 = ISA_Print_Type_Create("print_17", "%5s %s psr.l=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r2
  Instruction_Print_Group(print_17,
	TOP_mov_t_psr,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s%s %s ===== */
  ISA_PRINT_TYPE print_18;
  print_18 = ISA_Print_Type_Create("print_18", "%5s %s%s%s%s %s");
  Operand(0);		// qp
  Name();
  Operand(1);		// bwh
  Operand(2);		// ph
  Operand(3);		// dh
  Operand(4);		// imm
  Instruction_Print_Group(print_18,
	TOP_br_cond,	TOP_br_ret,	TOP_br_wexit,
	TOP_br_wtop,	TOP_br_r_cond,	TOP_brl_cond,
	TOP_UNDEFINED);

/* ===== %5s %s %s=psr.um ===== */
  ISA_PRINT_TYPE print_19;
  print_19 = ISA_Print_Type_Create("print_19", "%5s %s %s=psr.um");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Instruction_Print_Group(print_19,
	TOP_mov_f_psrum,
	TOP_UNDEFINED);

/* ===== %5s %s %s=rr[%s] ===== */
  ISA_PRINT_TYPE print_20;
  print_20 = ISA_Print_Type_Create("print_20", "%5s %s %s=rr[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_20,
	TOP_mov_f_rr,
	TOP_UNDEFINED);

/* ===== %5s %s pr.rot=%s ===== */
  ISA_PRINT_TYPE print_21;
  print_21 = ISA_Print_Type_Create("print_21", "%5s %s pr.rot=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// imm
  Instruction_Print_Group(print_21,
	TOP_mov_t_pr_i,
	TOP_UNDEFINED);

/* ===== %5s %s psr.um=%s ===== */
  ISA_PRINT_TYPE print_22;
  print_22 = ISA_Print_Type_Create("print_22", "%5s %s psr.um=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r2
  Instruction_Print_Group(print_22,
	TOP_mov_t_psrum,
	TOP_UNDEFINED);

/* ===== %5s %s rr[%s]=%s ===== */
  ISA_PRINT_TYPE print_23;
  print_23 = ISA_Print_Type_Create("print_23", "%5s %s rr[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_23,
	TOP_mov_t_rr,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s=[%s] ===== */
  ISA_PRINT_TYPE print_24;
  print_24 = ISA_Print_Type_Create("print_24", "%5s %s%s %s=[%s]");
  Operand(0);		// qp
  Name();
  Operand(1);		// ldhint
  Result(0);		// r1
  Operand(2);		// r3
  Instruction_Print_Group(print_24,
	TOP_ld8_fill,	TOP_ldf_fill,
	TOP_UNDEFINED);

/* ===== %5s %s%s [%s],%s ===== */
  ISA_PRINT_TYPE print_25;
  print_25 = ISA_Print_Type_Create("print_25", "%5s %s%s [%s],%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// lfhint
  Operand(2);		// r3
  Operand(3);		// imm
  Instruction_Print_Group(print_25,
	TOP_lfetch_i,	TOP_lfetch_i_excl,	TOP_lfetch_i_fault,
	TOP_lfetch_i_fault_excl,	TOP_lfetch_r,	TOP_lfetch_r_excl,
	TOP_lfetch_r_fault,	TOP_lfetch_r_fault_excl,
	TOP_UNDEFINED);

/* ===== %5s %s%s [%s]=%s ===== */
  ISA_PRINT_TYPE print_26;
  print_26 = ISA_Print_Type_Create("print_26", "%5s %s%s [%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sthint
  Operand(2);		// r3
  Operand(3);		// r2
  Instruction_Print_Group(print_26,
	TOP_st8_spill,	TOP_stf_spill,	TOP_stf8,
	TOP_stfd,	TOP_stfe,	TOP_stfs,
	TOP_UNDEFINED);

/* =====       %s%s%s%s %s ===== */
  ISA_PRINT_TYPE print_27;
  print_27 = ISA_Print_Type_Create("print_27", "      %s%s%s%s %s");
  Name();
  Operand(0);		// bwh
  Operand(1);		// ph
  Operand(2);		// dh
  Operand(3);		// imm
  Instruction_Print_Group(print_27,
	TOP_br_cexit,	TOP_br_cloop,	TOP_br_ctop,
	TOP_br_ia,
	TOP_UNDEFINED);

/* ===== %5s %s %s=%s,%s,1 ===== */
  ISA_PRINT_TYPE print_28;
  print_28 = ISA_Print_Type_Create("print_28", "%5s %s %s=%s,%s,1");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r2
  Operand(2);		// r3
  Instruction_Print_Group(print_28,
	TOP_add_1,	TOP_sub_1,
	TOP_UNDEFINED);

/* ===== %5s %s %s=dbr[%s] ===== */
  ISA_PRINT_TYPE print_29;
  print_29 = ISA_Print_Type_Create("print_29", "%5s %s %s=dbr[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_29,
	TOP_mov_f_dbr,
	TOP_UNDEFINED);

/* ===== %5s %s %s=ibr[%s] ===== */
  ISA_PRINT_TYPE print_30;
  print_30 = ISA_Print_Type_Create("print_30", "%5s %s %s=ibr[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_30,
	TOP_mov_f_ibr,
	TOP_UNDEFINED);

/* ===== %5s %s %s=msr[%s] ===== */
  ISA_PRINT_TYPE print_31;
  print_31 = ISA_Print_Type_Create("print_31", "%5s %s %s=msr[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_31,
	TOP_mov_f_msr,
	TOP_UNDEFINED);

/* ===== %5s %s %s=pkr[%s] ===== */
  ISA_PRINT_TYPE print_32;
  print_32 = ISA_Print_Type_Create("print_32", "%5s %s %s=pkr[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_32,
	TOP_mov_f_pkr,
	TOP_UNDEFINED);

/* ===== %5s %s %s=pmc[%s] ===== */
  ISA_PRINT_TYPE print_33;
  print_33 = ISA_Print_Type_Create("print_33", "%5s %s %s=pmc[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_33,
	TOP_mov_f_pmc,
	TOP_UNDEFINED);

/* ===== %5s %s %s=pmd[%s] ===== */
  ISA_PRINT_TYPE print_34;
  print_34 = ISA_Print_Type_Create("print_34", "%5s %s %s=pmd[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_34,
	TOP_mov_f_pmd,
	TOP_UNDEFINED);

/* ===== %5s %s dbr[%s]=%s ===== */
  ISA_PRINT_TYPE print_35;
  print_35 = ISA_Print_Type_Create("print_35", "%5s %s dbr[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_35,
	TOP_mov_t_dbr,
	TOP_UNDEFINED);

/* ===== %5s %s dtr[%s]=%s ===== */
  ISA_PRINT_TYPE print_36;
  print_36 = ISA_Print_Type_Create("print_36", "%5s %s dtr[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_36,
	TOP_itr_d,
	TOP_UNDEFINED);

/* ===== %5s %s ibr[%s]=%s ===== */
  ISA_PRINT_TYPE print_37;
  print_37 = ISA_Print_Type_Create("print_37", "%5s %s ibr[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_37,
	TOP_mov_t_ibr,
	TOP_UNDEFINED);

/* ===== %5s %s itr[%s]=%s ===== */
  ISA_PRINT_TYPE print_38;
  print_38 = ISA_Print_Type_Create("print_38", "%5s %s itr[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_38,
	TOP_itr_i,
	TOP_UNDEFINED);

/* ===== %5s %s msr[%s]=%s ===== */
  ISA_PRINT_TYPE print_39;
  print_39 = ISA_Print_Type_Create("print_39", "%5s %s msr[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_39,
	TOP_mov_t_msr,
	TOP_UNDEFINED);

/* ===== %5s %s pkr[%s]=%s ===== */
  ISA_PRINT_TYPE print_40;
  print_40 = ISA_Print_Type_Create("print_40", "%5s %s pkr[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_40,
	TOP_mov_t_pkr,
	TOP_UNDEFINED);

/* ===== %5s %s pmc[%s]=%s ===== */
  ISA_PRINT_TYPE print_41;
  print_41 = ISA_Print_Type_Create("print_41", "%5s %s pmc[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_41,
	TOP_mov_t_pmc,
	TOP_UNDEFINED);

/* ===== %5s %s pmd[%s]=%s ===== */
  ISA_PRINT_TYPE print_42;
  print_42 = ISA_Print_Type_Create("print_42", "%5s %s pmd[%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// r3
  Operand(2);		// r2
  Instruction_Print_Group(print_42,
	TOP_mov_t_pmd,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s,%s=%s ===== */
  ISA_PRINT_TYPE print_43;
  print_43 = ISA_Print_Type_Create("print_43", "%5s %s%s %s,%s=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sf
  Result(0);		// f1
  Result(1);		// p2
  Operand(2);		// f3
  Instruction_Print_Group(print_43,
	TOP_fprsqrta,	TOP_frsqrta,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s=%s,%s ===== */
  ISA_PRINT_TYPE print_44;
  print_44 = ISA_Print_Type_Create("print_44", "%5s %s%s %s=%s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sf
  Result(0);		// f1
  Operand(2);		// f3
  Operand(3);		// f2
  Instruction_Print_Group(print_44,
	TOP_fadd,	TOP_fadd_d,	TOP_fadd_s,
	TOP_famax,	TOP_famin,	TOP_fmax,
	TOP_fmin,	TOP_fmpy,	TOP_fmpy_d,
	TOP_fmpy_s,	TOP_fnmpy,	TOP_fnmpy_d,
	TOP_fnmpy_s,	TOP_fpamax,	TOP_fpamin,
	TOP_fpcmp_eq,	TOP_fpcmp_ge,	TOP_fpcmp_gt,
	TOP_fpcmp_le,	TOP_fpcmp_lt,	TOP_fpcmp_neq,
	TOP_fpcmp_nge,	TOP_fpcmp_ngt,	TOP_fpcmp_nle,
	TOP_fpcmp_nlt,	TOP_fpcmp_ord,	TOP_fpcmp_unord,
	TOP_fpmax,	TOP_fpmin,	TOP_fpmpy,
	TOP_fpnmpy,	TOP_fsub,	TOP_fsub_d,
	TOP_fsub_s,
	TOP_UNDEFINED);

/* =====       %s%s%s %s,%s ===== */
  ISA_PRINT_TYPE print_45;
  print_45 = ISA_Print_Type_Create("print_45", "      %s%s%s %s,%s");
  Name();
  Operand(0);		// ipwh
  Operand(1);		// ih
  Operand(2);		// imm
  Operand(3);		// imm
  Instruction_Print_Group(print_45,
	TOP_brp,	TOP_brp_ret,	TOP_brp_r,
	TOP_UNDEFINED);

/* ===== %5s %s %s,%s=%s,%s ===== */
  ISA_PRINT_TYPE print_46;
  print_46 = ISA_Print_Type_Create("print_46", "%5s %s %s,%s=%s,%s");
  Operand(0);		// qp
  Name();
  Result(0);		// p1
  Result(1);		// p2
  Operand(1);		// r2
  Operand(2);		// r3
  Instruction_Print_Group(print_46,
	TOP_cmp_eq,	TOP_cmp_eq_and,	TOP_cmp_eq_and_orcm,
	TOP_cmp_eq_andcm,	TOP_cmp_eq_or,	TOP_cmp_eq_or_andcm,
	TOP_cmp_eq_orcm,	TOP_cmp_eq_unc,	TOP_cmp_ge,
	TOP_cmp_ge_unc,	TOP_cmp_geu,	TOP_cmp_geu_unc,
	TOP_cmp_gt,	TOP_cmp_gt_unc,	TOP_cmp_gtu,
	TOP_cmp_gtu_unc,	TOP_cmp_le,	TOP_cmp_le_unc,
	TOP_cmp_leu,	TOP_cmp_leu_unc,	TOP_cmp_lt,
	TOP_cmp_lt_unc,	TOP_cmp_ltu,	TOP_cmp_ltu_unc,
	TOP_cmp_ne,	TOP_cmp_ne_and,	TOP_cmp_ne_and_orcm,
	TOP_cmp_ne_andcm,	TOP_cmp_ne_or,	TOP_cmp_ne_or_andcm,
	TOP_cmp_ne_orcm,	TOP_cmp_ne_unc,	TOP_cmp4_eq,
	TOP_cmp4_eq_and,	TOP_cmp4_eq_and_orcm,	TOP_cmp4_eq_andcm,
	TOP_cmp4_eq_or,	TOP_cmp4_eq_or_andcm,	TOP_cmp4_eq_orcm,
	TOP_cmp4_eq_unc,	TOP_cmp4_ge,	TOP_cmp4_ge_unc,
	TOP_cmp4_geu,	TOP_cmp4_geu_unc,	TOP_cmp4_gt,
	TOP_cmp4_gt_unc,	TOP_cmp4_gtu,	TOP_cmp4_gtu_unc,
	TOP_cmp4_le,	TOP_cmp4_le_unc,	TOP_cmp4_leu,
	TOP_cmp4_leu_unc,	TOP_cmp4_lt,	TOP_cmp4_lt_unc,
	TOP_cmp4_ltu,	TOP_cmp4_ltu_unc,	TOP_cmp4_ne,
	TOP_cmp4_ne_and,	TOP_cmp4_ne_and_orcm,	TOP_cmp4_ne_andcm,
	TOP_cmp4_ne_or,	TOP_cmp4_ne_or_andcm,	TOP_cmp4_ne_orcm,
	TOP_cmp4_ne_unc,	TOP_cmp4_i_eq,	TOP_cmp4_i_eq_and,
	TOP_cmp4_i_eq_and_orcm,	TOP_cmp4_i_eq_andcm,	TOP_cmp4_i_eq_or,
	TOP_cmp4_i_eq_or_andcm,	TOP_cmp4_i_eq_orcm,	TOP_cmp4_i_eq_unc,
	TOP_cmp4_i_ge,	TOP_cmp4_i_ge_unc,	TOP_cmp4_i_geu,
	TOP_cmp4_i_geu_unc,	TOP_cmp4_i_gt,	TOP_cmp4_i_gt_unc,
	TOP_cmp4_i_gtu,	TOP_cmp4_i_gtu_unc,	TOP_cmp4_i_le,
	TOP_cmp4_i_le_unc,	TOP_cmp4_i_leu,	TOP_cmp4_i_leu_unc,
	TOP_cmp4_i_lt,	TOP_cmp4_i_lt_unc,	TOP_cmp4_i_ltu,
	TOP_cmp4_i_ltu_unc,	TOP_cmp4_i_ne,	TOP_cmp4_i_ne_and,
	TOP_cmp4_i_ne_and_orcm,	TOP_cmp4_i_ne_andcm,	TOP_cmp4_i_ne_or,
	TOP_cmp4_i_ne_or_andcm,	TOP_cmp4_i_ne_orcm,	TOP_cmp4_i_ne_unc,
	TOP_cmp_i_eq,	TOP_cmp_i_eq_and,	TOP_cmp_i_eq_and_orcm,
	TOP_cmp_i_eq_andcm,	TOP_cmp_i_eq_or,	TOP_cmp_i_eq_or_andcm,
	TOP_cmp_i_eq_orcm,	TOP_cmp_i_eq_unc,	TOP_cmp_i_ge,
	TOP_cmp_i_ge_unc,	TOP_cmp_i_geu,	TOP_cmp_i_geu_unc,
	TOP_cmp_i_gt,	TOP_cmp_i_gt_unc,	TOP_cmp_i_gtu,
	TOP_cmp_i_gtu_unc,	TOP_cmp_i_le,	TOP_cmp_i_le_unc,
	TOP_cmp_i_leu,	TOP_cmp_i_leu_unc,	TOP_cmp_i_lt,
	TOP_cmp_i_lt_unc,	TOP_cmp_i_ltu,	TOP_cmp_i_ltu_unc,
	TOP_cmp_i_ne,	TOP_cmp_i_ne_and,	TOP_cmp_i_ne_and_orcm,
	TOP_cmp_i_ne_andcm,	TOP_cmp_i_ne_or,	TOP_cmp_i_ne_or_andcm,
	TOP_cmp_i_ne_orcm,	TOP_cmp_i_ne_unc,	TOP_fclass_m,
	TOP_fclass_m_unc,	TOP_fclass_nm,	TOP_fclass_nm_unc,
	TOP_tbit_nz,	TOP_tbit_nz_and,	TOP_tbit_nz_or,
	TOP_tbit_nz_or_andcm,	TOP_tbit_nz_unc,	TOP_tbit_z,
	TOP_tbit_z_and,	TOP_tbit_z_or,	TOP_tbit_z_or_andcm,
	TOP_tbit_z_unc,
	TOP_UNDEFINED);

/* ===== %5s %s %s,%s=%s,r0 ===== */
  ISA_PRINT_TYPE print_47;
  print_47 = ISA_Print_Type_Create("print_47", "%5s %s %s,%s=%s,r0");
  Operand(0);		// qp
  Name();
  Result(0);		// p1
  Result(1);		// p2
  Operand(1);		// r3
  Instruction_Print_Group(print_47,
	TOP_cmp4_z2_ge_and,	TOP_cmp4_z2_ge_and_orcm,	TOP_cmp4_z2_ge_andcm,
	TOP_cmp4_z2_ge_or,	TOP_cmp4_z2_ge_or_andcm,	TOP_cmp4_z2_ge_orcm,
	TOP_cmp4_z2_gt_and,	TOP_cmp4_z2_gt_and_orcm,	TOP_cmp4_z2_gt_andcm,
	TOP_cmp4_z2_gt_or,	TOP_cmp4_z2_gt_or_andcm,	TOP_cmp4_z2_gt_orcm,
	TOP_cmp4_z2_le_and,	TOP_cmp4_z2_le_and_orcm,	TOP_cmp4_z2_le_andcm,
	TOP_cmp4_z2_le_or,	TOP_cmp4_z2_le_or_andcm,	TOP_cmp4_z2_le_orcm,
	TOP_cmp4_z2_lt_and,	TOP_cmp4_z2_lt_and_orcm,	TOP_cmp4_z2_lt_andcm,
	TOP_cmp4_z2_lt_or,	TOP_cmp4_z2_lt_or_andcm,	TOP_cmp4_z2_lt_orcm,
	TOP_cmp_z2_ge_and,	TOP_cmp_z2_ge_and_orcm,	TOP_cmp_z2_ge_andcm,
	TOP_cmp_z2_ge_or,	TOP_cmp_z2_ge_or_andcm,	TOP_cmp_z2_ge_orcm,
	TOP_cmp_z2_gt_and,	TOP_cmp_z2_gt_and_orcm,	TOP_cmp_z2_gt_andcm,
	TOP_cmp_z2_gt_or,	TOP_cmp_z2_gt_or_andcm,	TOP_cmp_z2_gt_orcm,
	TOP_cmp_z2_le_and,	TOP_cmp_z2_le_and_orcm,	TOP_cmp_z2_le_andcm,
	TOP_cmp_z2_le_or,	TOP_cmp_z2_le_or_andcm,	TOP_cmp_z2_le_orcm,
	TOP_cmp_z2_lt_and,	TOP_cmp_z2_lt_and_orcm,	TOP_cmp_z2_lt_andcm,
	TOP_cmp_z2_lt_or,	TOP_cmp_z2_lt_or_andcm,	TOP_cmp_z2_lt_orcm,
	TOP_UNDEFINED);

/* ===== %5s %s %s,%s=r0,%s ===== */
  ISA_PRINT_TYPE print_48;
  print_48 = ISA_Print_Type_Create("print_48", "%5s %s %s,%s=r0,%s");
  Operand(0);		// qp
  Name();
  Result(0);		// p1
  Result(1);		// p2
  Operand(1);		// r3
  Instruction_Print_Group(print_48,
	TOP_cmp4_z1_ge_and,	TOP_cmp4_z1_ge_and_orcm,	TOP_cmp4_z1_ge_andcm,
	TOP_cmp4_z1_ge_or,	TOP_cmp4_z1_ge_or_andcm,	TOP_cmp4_z1_ge_orcm,
	TOP_cmp4_z1_gt_and,	TOP_cmp4_z1_gt_and_orcm,	TOP_cmp4_z1_gt_andcm,
	TOP_cmp4_z1_gt_or,	TOP_cmp4_z1_gt_or_andcm,	TOP_cmp4_z1_gt_orcm,
	TOP_cmp4_z1_le_and,	TOP_cmp4_z1_le_and_orcm,	TOP_cmp4_z1_le_andcm,
	TOP_cmp4_z1_le_or,	TOP_cmp4_z1_le_or_andcm,	TOP_cmp4_z1_le_orcm,
	TOP_cmp4_z1_lt_and,	TOP_cmp4_z1_lt_and_orcm,	TOP_cmp4_z1_lt_andcm,
	TOP_cmp4_z1_lt_or,	TOP_cmp4_z1_lt_or_andcm,	TOP_cmp4_z1_lt_orcm,
	TOP_cmp_z1_ge_and,	TOP_cmp_z1_ge_and_orcm,	TOP_cmp_z1_ge_andcm,
	TOP_cmp_z1_ge_or,	TOP_cmp_z1_ge_or_andcm,	TOP_cmp_z1_ge_orcm,
	TOP_cmp_z1_gt_and,	TOP_cmp_z1_gt_and_orcm,	TOP_cmp_z1_gt_andcm,
	TOP_cmp_z1_gt_or,	TOP_cmp_z1_gt_or_andcm,	TOP_cmp_z1_gt_orcm,
	TOP_cmp_z1_le_and,	TOP_cmp_z1_le_and_orcm,	TOP_cmp_z1_le_andcm,
	TOP_cmp_z1_le_or,	TOP_cmp_z1_le_or_andcm,	TOP_cmp_z1_le_orcm,
	TOP_cmp_z1_lt_and,	TOP_cmp_z1_lt_and_orcm,	TOP_cmp_z1_lt_andcm,
	TOP_cmp_z1_lt_or,	TOP_cmp_z1_lt_or_andcm,	TOP_cmp_z1_lt_orcm,
	TOP_UNDEFINED);

/* ===== %5s %s %s=%s,%s,%s ===== */
  ISA_PRINT_TYPE print_49;
  print_49 = ISA_Print_Type_Create("print_49", "%5s %s %s=%s,%s,%s");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r2
  Operand(2);		// imm
  Operand(3);		// imm
  Instruction_Print_Group(print_49,
	TOP_dep_z,	TOP_dep_i_z,	TOP_extr,
	TOP_extr_u,	TOP_fselect,	TOP_pmpyshr2,
	TOP_pmpyshr2_u,	TOP_pshladd2,	TOP_pshradd2,
	TOP_shladd,	TOP_shladdp4,	TOP_shrp,
	TOP_xma_h,	TOP_xma_hu,	TOP_xma_l,
	TOP_xma_lu,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s %s=[%s] ===== */
  ISA_PRINT_TYPE print_50;
  print_50 = ISA_Print_Type_Create("print_50", "%5s %s%s%s %s=[%s]");
  Operand(0);		// qp
  Name();
  Operand(1);		// ldtype
  Operand(2);		// ldhint
  Result(0);		// r1
  Operand(3);		// r3
  Instruction_Print_Group(print_50,
	TOP_ld1,	TOP_ld2,	TOP_ld4,
	TOP_ld8,	TOP_ldf8,	TOP_ldfd,
	TOP_ldfe,	TOP_ldfs,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s [%s]=%s ===== */
  ISA_PRINT_TYPE print_51;
  print_51 = ISA_Print_Type_Create("print_51", "%5s %s%s%s [%s]=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sttype
  Operand(2);		// sthint
  Operand(3);		// r3
  Operand(4);		// r2
  Instruction_Print_Group(print_51,
	TOP_st1,	TOP_st2,	TOP_st4,
	TOP_st8,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s%s %s=%s ===== */
  ISA_PRINT_TYPE print_52;
  print_52 = ISA_Print_Type_Create("print_52", "%5s %s%s%s%s %s=%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// bwh
  Operand(2);		// ph
  Operand(3);		// dh
  Result(0);		// b1
  Operand(4);		// imm
  Instruction_Print_Group(print_52,
	TOP_br_call,	TOP_br_r_call,	TOP_brl_call,
	TOP_UNDEFINED);

/* ===== %5s %s %s=cpuid[%s] ===== */
  ISA_PRINT_TYPE print_53;
  print_53 = ISA_Print_Type_Create("print_53", "%5s %s %s=cpuid[%s]");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r3
  Instruction_Print_Group(print_53,
	TOP_mov_f_cpuid,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s=[%s],%s ===== */
  ISA_PRINT_TYPE print_54;
  print_54 = ISA_Print_Type_Create("print_54", "%5s %s%s %s=[%s],%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// ldhint
  Result(0);		// r1
  Operand(2);		// r3
  Operand(3);		// imm
  Instruction_Print_Group(print_54,
	TOP_ld8_i_fill,	TOP_ld8_r_fill,	TOP_ldf_i_fill,
	TOP_ldf_r_fill,	TOP_xchg1,	TOP_xchg2,
	TOP_xchg4,	TOP_xchg8,
	TOP_UNDEFINED);

/* ===== %5s %s%s [%s]=%s,%s ===== */
  ISA_PRINT_TYPE print_55;
  print_55 = ISA_Print_Type_Create("print_55", "%5s %s%s [%s]=%s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sthint
  Operand(2);		// r3
  Operand(3);		// r2
  Operand(4);		// imm
  Instruction_Print_Group(print_55,
	TOP_st8_i_spill,	TOP_stf8_i,	TOP_stf_i_spill,
	TOP_stfd_i,	TOP_stfe_i,	TOP_stfs_i,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s %s=%s,%s ===== */
  ISA_PRINT_TYPE print_56;
  print_56 = ISA_Print_Type_Create("print_56", "%5s %s%s%s %s=%s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// mwh
  Operand(2);		// ih
  Result(0);		// b1
  Operand(3);		// r2
  Operand(4);		// imm
  Instruction_Print_Group(print_56,
	TOP_mov_t_br_ret,	TOP_mov_t_br_i,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s,%s=%s,%s ===== */
  ISA_PRINT_TYPE print_57;
  print_57 = ISA_Print_Type_Create("print_57", "%5s %s%s %s,%s=%s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sf
  Result(0);		// p1
  Result(1);		// p2
  Operand(2);		// f2
  Operand(3);		// f3
  Instruction_Print_Group(print_57,
	TOP_fcmp_eq,	TOP_fcmp_eq_unc,	TOP_fcmp_ge,
	TOP_fcmp_ge_unc,	TOP_fcmp_gt,	TOP_fcmp_gt_unc,
	TOP_fcmp_le,	TOP_fcmp_le_unc,	TOP_fcmp_lt,
	TOP_fcmp_lt_unc,	TOP_fcmp_neq,	TOP_fcmp_neq_unc,
	TOP_fcmp_nge,	TOP_fcmp_nge_unc,	TOP_fcmp_ngt,
	TOP_fcmp_ngt_unc,	TOP_fcmp_nle,	TOP_fcmp_nle_unc,
	TOP_fcmp_nlt,	TOP_fcmp_nlt_unc,	TOP_fcmp_ord,
	TOP_fcmp_ord_unc,	TOP_fcmp_unord,	TOP_fcmp_unord_unc,
	TOP_fprcpa,	TOP_frcpa,
	TOP_UNDEFINED);

/* ===== %5s %s%s %s=%s,%s,%s ===== */
  ISA_PRINT_TYPE print_58;
  print_58 = ISA_Print_Type_Create("print_58", "%5s %s%s %s=%s,%s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sf
  Result(0);		// f1
  Operand(2);		// f3
  Operand(3);		// f4
  Operand(4);		// f2
  Instruction_Print_Group(print_58,
	TOP_fma,	TOP_fma_d,	TOP_fma_s,
	TOP_fms,	TOP_fms_d,	TOP_fms_s,
	TOP_fnma,	TOP_fnma_d,	TOP_fnma_s,
	TOP_fpma,	TOP_fpms,	TOP_fpnma,
	TOP_UNDEFINED);

/* ===== %5s %s %s=%s,%s,%s,%s ===== */
  ISA_PRINT_TYPE print_59;
  print_59 = ISA_Print_Type_Create("print_59", "%5s %s %s=%s,%s,%s,%s");
  Operand(0);		// qp
  Name();
  Result(0);		// r1
  Operand(1);		// r2
  Operand(2);		// r3
  Operand(3);		// imm
  Operand(4);		// imm
  Instruction_Print_Group(print_59,
	TOP_dep,	TOP_dep_i,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s %s,%s=[%s] ===== */
  ISA_PRINT_TYPE print_60;
  print_60 = ISA_Print_Type_Create("print_60", "%5s %s%s%s %s,%s=[%s]");
  Operand(0);		// qp
  Name();
  Operand(1);		// fldtype
  Operand(2);		// ldhint
  Result(0);		// f1
  Result(1);		// f2
  Operand(3);		// r3
  Instruction_Print_Group(print_60,
	TOP_ldfp8,	TOP_ldfpd,	TOP_ldfps,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s %s=[%s],%s ===== */
  ISA_PRINT_TYPE print_61;
  print_61 = ISA_Print_Type_Create("print_61", "%5s %s%s%s %s=[%s],%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sem
  Operand(2);		// ldhint
  Result(0);		// r1
  Operand(3);		// r3
  Operand(4);		// imm
  Instruction_Print_Group(print_61,
	TOP_fetchadd4,	TOP_fetchadd8,	TOP_ld1_i,
	TOP_ld1_r,	TOP_ld2_i,	TOP_ld2_r,
	TOP_ld4_i,	TOP_ld4_r,	TOP_ld8_i,
	TOP_ld8_r,	TOP_ldf8_i,	TOP_ldf8_r,
	TOP_ldfd_i,	TOP_ldfd_r,	TOP_ldfe_i,
	TOP_ldfe_r,	TOP_ldfs_i,	TOP_ldfs_r,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s [%s]=%s,%s ===== */
  ISA_PRINT_TYPE print_62;
  print_62 = ISA_Print_Type_Create("print_62", "%5s %s%s%s [%s]=%s,%s");
  Operand(0);		// qp
  Name();
  Operand(1);		// sttype
  Operand(2);		// sthint
  Operand(3);		// r3
  Operand(4);		// r2
  Operand(5);		// imm
  Instruction_Print_Group(print_62,
	TOP_st1_i,	TOP_st2_i,	TOP_st4_i,
	TOP_st8_i,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s %s,%s=[%s],8 ===== */
  ISA_PRINT_TYPE print_63;
  print_63 = ISA_Print_Type_Create("print_63", "%5s %s%s%s %s,%s=[%s],8");
  Operand(0);		// qp
  Name();
  Operand(1);		// fldtype
  Operand(2);		// ldhint
  Result(0);		// f1
  Result(1);		// f2
  Operand(3);		// r3
  Instruction_Print_Group(print_63,
	TOP_ldfps_i,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s %s,%s=[%s],16 ===== */
  ISA_PRINT_TYPE print_64;
  print_64 = ISA_Print_Type_Create("print_64", "%5s %s%s%s %s,%s=[%s],16");
  Operand(0);		// qp
  Name();
  Operand(1);		// fldtype
  Operand(2);		// ldhint
  Result(0);		// f1
  Result(1);		// f2
  Operand(3);		// r3
  Instruction_Print_Group(print_64,
	TOP_ldfp8_i,	TOP_ldfpd_i,
	TOP_UNDEFINED);

/* =====       %s %s=ar.pfs,%s,%s,%s ===== */
  ISA_PRINT_TYPE print_65;
  print_65 = ISA_Print_Type_Create("print_65", "      %s %s=ar.pfs,%s,%s,%s");
  Name();
  Result(0);		// r1
  Operand(0);		// imm
  Operand(1);		// imm
  Operand(2);		// imm
  Instruction_Print_Group(print_65,
	TOP_alloc_3,
	TOP_UNDEFINED);

/* ===== %5s %s%s%s %s=[%s],%s,ar.ccv ===== */
  ISA_PRINT_TYPE print_66;
  print_66 = ISA_Print_Type_Create("print_66", "%5s %s%s%s %s=[%s],%s,ar.ccv");
  Operand(0);		// qp
  Name();
  Operand(1);		// sem
  Operand(2);		// ldhint
  Result(0);		// r1
  Operand(3);		// r3
  Operand(4);		// r2
  Instruction_Print_Group(print_66,
	TOP_cmpxchg1,	TOP_cmpxchg2,	TOP_cmpxchg4,
	TOP_cmpxchg8,
	TOP_UNDEFINED);

/* =====       %s %s=ar.pfs,%s,%s,%s,%s ===== */
  ISA_PRINT_TYPE print_67;
  print_67 = ISA_Print_Type_Create("print_67", "      %s %s=ar.pfs,%s,%s,%s,%s");
  Name();
  Result(0);		// r1
  Operand(0);		// imm
  Operand(1);		// imm
  Operand(2);		// imm
  Operand(3);		// imm
  Instruction_Print_Group(print_67,
	TOP_alloc,
	TOP_UNDEFINED);

  ISA_Print_End();
  return 0;
}
