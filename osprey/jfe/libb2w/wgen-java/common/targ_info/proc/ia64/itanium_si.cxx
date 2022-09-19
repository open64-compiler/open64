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


//  Intel Itanium (tm) processor scheduling information
/////////////////////////////////////
//  
//  Description:
//
//  Generate a scheduling description of a Intel Itanium (tm) processor
//  via the si_gen interface.
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/proc/ia64/itanium_si.cxx,v $


#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_issue,
		res_sem,	// for non-pipelinable semaphore insts
                res_memory,	// Memory unit 
                res_M0,		// M0 Memory unit
                res_float,	// Float unit
                res_F0,		// F0 Float unit
                res_integer,	// Integer unit
                res_I0,		// I0 unit
                res_int_or_mem,	// to simulate A-type insts which can execute
				// in either I or M unit
                res_branch,	// Branch unit
		res_B0_or_B1,	// B0 or B1 unit
		res_B0_or_B2,	// B0 or B2 unit
		res_B2;		// B2 unit

int
main (int argc, char *argv[])
{
  Machine("itanium", ISA_SUBSET_intel1, argc, argv);

  res_issue = RESOURCE_Create("issue", 6);
  res_sem = RESOURCE_Create("sem", 1);
  res_int_or_mem = RESOURCE_Create("integer_or_memory", 4);
  res_memory = RESOURCE_Create("memory", 2);
  res_M0 = RESOURCE_Create("memory0", 1);
  res_float = RESOURCE_Create("floating-point", 2);
  res_F0 = RESOURCE_Create("floating-point0", 1);
  res_integer = RESOURCE_Create("integer", 2);
  res_I0 = RESOURCE_Create("integer0", 1);
  res_branch = RESOURCE_Create("branch", 3);
  res_B0_or_B1 = RESOURCE_Create("B0_or_B1", 2);
  res_B0_or_B2 = RESOURCE_Create("B0_or_B2", 2);
  res_B2 = RESOURCE_Create("B2", 1);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the BR functional unit class:
  // B-type instructions which can execute in any B unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("BR",
		    TOP_br_call,
		    TOP_br_r_call,
		    TOP_brl_call,
		    TOP_br_cond,
		    TOP_br_r_cond,
		    TOP_brl_cond,
		    TOP_br_ia,
		    TOP_br_ret,
		    TOP_br,		// pseudo of br_cond
		    TOP_br_r,		// pseudo of br_r_cond
		    TOP_brl,		// pseudo of brl_cond
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// ??? not sure
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the BR_B2 functional unit class:
  // B-type instructions which can execute in any B unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("BR_B2",
		    TOP_br_cexit,
		    TOP_br_cloop,
		    TOP_br_ctop,
		    TOP_br_wexit,	// ??? correct functional unit?
		    TOP_br_wtop,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// ??? not sure
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);
  Resource_Requirement(res_B2, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the BRP functional unit class:
  // B-type instructions which can exeucte in any B unit, but
  // if sent to B1 will be ignored.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("BRP",
		    TOP_brp,
		    TOP_brp_r,
		    TOP_brp_ret,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the CHK_ALAT functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("CHK_ALAT",
		    TOP_chk_a,
		    TOP_chk_f_a,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the CHK_I functional unit class:
  // I-type instructions which can execute in any I Unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("CHK_I",
		    TOP_chk_s_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the CHK_M functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("CHK_M",
		    TOP_chk_f_s,
		    TOP_chk_s_m,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the CLD functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("CLD",
		    // ??? ld.c
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(2);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FCLD functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FCLD",
		    // ??? ldf8.c
		    // ??? ldfd.c
		    // ??? ldfe.c
		    // ??? ldfp8.c
		    // ??? ldfpd.c
		    // ??? ldfps.c
		    // ??? ldfs.c
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(2);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FCMP functional unit class:
  // F-type instructions which can only execute in F0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FCMP",
		    TOP_fclass_nm,
		    TOP_fclass_nm_unc,
		    TOP_fclass_m,
		    TOP_fclass_m_unc,
		    TOP_fcmp_eq,
		    TOP_fcmp_lt,
		    TOP_fcmp_le,
		    TOP_fcmp_unord,
		    TOP_fcmp_eq_unc,
		    TOP_fcmp_lt_unc,
		    TOP_fcmp_le_unc,
		    TOP_fcmp_unord_unc,
		    TOP_fcmp_gt,
		    TOP_fcmp_gt_unc,
		    TOP_fcmp_ge,
		    TOP_fcmp_ge_unc,
		    TOP_fcmp_neq,
		    TOP_fcmp_neq_unc,
		    TOP_fcmp_nlt,
		    TOP_fcmp_nlt_unc,
		    TOP_fcmp_nle,
		    TOP_fcmp_nle_unc,
		    TOP_fcmp_ngt,
		    TOP_fcmp_ngt_unc,
		    TOP_fcmp_nge,
		    TOP_fcmp_nge_unc,
		    TOP_fcmp_ord,
		    TOP_fcmp_ord_unc,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +2 to dependent SFxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0);	// can only execute in F0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FCVTFX functional unit class:
  // F-type instructions which can execute in any F unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FCVTFX",
		    TOP_fcvt_fx,
		    TOP_fcvt_fx_trunc,
		    TOP_fcvt_fxu,
		    TOP_fcvt_fxu_trunc,
		    TOP_fcvt_xf,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);		// +2 to dependent SFxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FLD functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FLD",
		    TOP_ldf8,
		    TOP_ldf8_r,
		    TOP_ldf8_i,
		    TOP_ldfd,
		    TOP_ldfd_r,
		    TOP_ldfd_i,
		    TOP_ldfe,
		    TOP_ldfe_r,
		    TOP_ldfe_i,
		    TOP_ldf_fill,
		    TOP_ldf_r_fill,
		    TOP_ldf_i_fill,
		    TOP_ldfs,
		    TOP_ldfs_r,
		    TOP_ldfs_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);		// +2 to dependent SFxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(9);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FLDP functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FLDP",
		    TOP_ldfp8,
		    TOP_ldfp8_i,
		    TOP_ldfpd,
		    TOP_ldfpd_i,
		    TOP_ldfps,
		    TOP_ldfps_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);		// +2 to dependent SFxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(9);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FMAC functional unit class:
  // F-type instructions which can execute in any F unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FMAC",
		    TOP_fma,
		    TOP_fma_s,
		    TOP_fma_d,
		    TOP_fms,
		    TOP_fms_s,
		    TOP_fms_d,
		    TOP_fnma,
		    TOP_fnma_s,
		    TOP_fnma_d,
		    TOP_fcvt_xuf,	// pseudo of fma
		    TOP_fcvt_xuf_d,	// pseudo of fma_d
		    TOP_fcvt_xuf_s,	// pseudo of fma_s
		    TOP_fadd,		// pseudo of fma
		    TOP_fadd_d,		// pseudo of fma_d
		    TOP_fadd_s,		// pseudo of fma_s
		    TOP_fmpy,		// pseudo of fma
		    TOP_fmpy_d,		// pseudo of fma_d
		    TOP_fmpy_s,		// pseudo of fma_s
		    TOP_fnmpy,		// pseudo of fnma
		    TOP_fnmpy_d,	// pseudo of fnma_d
		    TOP_fnmpy_s,	// pseudo of fnma_s
		    TOP_fnorm,		// pseudo of fma
		    TOP_fnorm_d,	// pseudo of fma_d
		    TOP_fnorm_s,	// pseudo of fma_s
		    TOP_fsub,		// pseudo of fms
		    TOP_fsub_d,		// pseudo of fms_d
		    TOP_fsub_s,		// pseudo of fms_s
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);		// +2 to dependent FMISC, FCVTFX, XMA
					// +2 to dependent SFxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FMISC functional unit class:
  // F-type instructions which can only execute in F0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FMISC",
		    TOP_famax,
		    TOP_famin,
		    TOP_fand,
		    TOP_fandcm,
		    TOP_fmax,
		    TOP_fmerge_ns,
		    TOP_fmerge_s,
		    TOP_fmerge_se,
		    TOP_fmin,
		    TOP_fmix_l,
		    TOP_fmix_lr,
		    TOP_fmix_r,
		    TOP_for,
		    TOP_fpack,
		    TOP_frcpa,
		    TOP_frsqrta,
		    TOP_fselect,
		    TOP_fswap,
		    TOP_fswap_nl,
		    TOP_fswap_nr,
		    TOP_fsxt_l,
		    TOP_fsxt_r,
		    TOP_fxor,
		    TOP_fabs,		// pseudo of fmerge_s
		    TOP_fnegabs,	// pseudo of fmerge_ns
		    TOP_fneg,		// pseudo of fmerge_ns
		    TOP_mov_f,		// pseudo of fmerge_s
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);		// +2 to dependent SFxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0);	// can only execute in F0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FOTHER functional unit class:
  // F-type instructions which can execute in any F unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FOTHER",
		    TOP_fchkf,
		    TOP_fclrf,
		    TOP_fsetc,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FRAR_I functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRAR_I",
		    TOP_mov_f_ar_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// depends on src register
					// +1 to dependent MMMUL, MMSHF, MMALU
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FRAR_M functional unit class:
  // M-type instructions which can only execute in M0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRAR_M",
		    TOP_mov_f_ar_m,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// depends on src register
					// +1 to dependent MMMUL, MMSHF, MMALU
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FRBR functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRBR",
		    TOP_mov_f_br,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +1 to dependent MMMUL, MMSHF, MMALU
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FRCR functional unit class:
  // M-type instructions which can only execute in M0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRCR",
		    TOP_mov_f_cr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// depends on src register
					// +1 to dependent MMMUL, MMSHF, MMALU
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FRFR functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRFR",
		    TOP_getf_sig,
		    TOP_getf_exp,
		    TOP_getf_s,
		    TOP_getf_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FRIP functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRIP",
		    TOP_mov_f_ip,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +1 to dependent MMMUL, MMSHF, MMALU
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the FRPR functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRPR",
		    TOP_mov_f_pr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the IALU functional unit class:
  // A-type instructions which can execute in any I or M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("IALU",
		    TOP_add,
		    TOP_add_1,
		    TOP_addl,
		    TOP_adds,
		    TOP_shladd,
		    TOP_sub,
		    TOP_sub_i,
		    TOP_sub_1,
		    TOP_mov_i,		// pseudo of addl
		    TOP_mov,		// pseudo of adds
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// +1 to dependent LD/ST base reg
					// (I-slot insts only)
					// +2 to dependent MMMUL, MMSHF, MMALU
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the ICMP functional unit class:
  // A-type instructions which can execute in any I or M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("ICMP",
		    TOP_cmp_eq,
		    TOP_cmp_eq_and,
		    TOP_cmp_eq_and_orcm,
		    TOP_cmp_eq_andcm,
		    TOP_cmp_eq_or,
		    TOP_cmp_eq_or_andcm,
		    TOP_cmp_eq_orcm,
		    TOP_cmp_eq_unc,
		    TOP_cmp_ge,
		    TOP_cmp_ge_and,
		    TOP_cmp_ge_and_orcm,
		    TOP_cmp_ge_andcm,
		    TOP_cmp_ge_or,
		    TOP_cmp_ge_or_andcm,
		    TOP_cmp_ge_orcm,
		    TOP_cmp_ge_unc,
		    TOP_cmp_geu,
		    TOP_cmp_geu_unc,
		    TOP_cmp_gt,
		    TOP_cmp_gt_and,
		    TOP_cmp_gt_and_orcm,
		    TOP_cmp_gt_andcm,
		    TOP_cmp_gt_or,
		    TOP_cmp_gt_or_andcm,
		    TOP_cmp_gt_orcm,
		    TOP_cmp_gt_unc,
		    TOP_cmp_gtu,
		    TOP_cmp_gtu_unc,
		    TOP_cmp_i_eq,
		    TOP_cmp_i_eq_and,
		    TOP_cmp_i_eq_and_orcm,
		    TOP_cmp_i_eq_andcm,
		    TOP_cmp_i_eq_or,
		    TOP_cmp_i_eq_or_andcm,
		    TOP_cmp_i_eq_orcm,
		    TOP_cmp_i_eq_unc,
		    TOP_cmp_i_ge,
		    TOP_cmp_i_ge_unc,
		    TOP_cmp_i_geu,
		    TOP_cmp_i_geu_unc,
		    TOP_cmp_i_gt,
		    TOP_cmp_i_gt_unc,
		    TOP_cmp_i_gtu,
		    TOP_cmp_i_gtu_unc,
		    TOP_cmp_i_le,
		    TOP_cmp_i_le_unc,
		    TOP_cmp_i_leu,
		    TOP_cmp_i_leu_unc,
		    TOP_cmp_i_lt,
		    TOP_cmp_i_lt_unc,
		    TOP_cmp_i_ltu,
		    TOP_cmp_i_ltu_unc,
		    TOP_cmp_i_ne,
		    TOP_cmp_i_ne_and,
		    TOP_cmp_i_ne_and_orcm,
		    TOP_cmp_i_ne_andcm,
		    TOP_cmp_i_ne_or,
		    TOP_cmp_i_ne_or_andcm,
		    TOP_cmp_i_ne_orcm,
		    TOP_cmp_i_ne_unc,
		    TOP_cmp_le,
		    TOP_cmp_le_and,
		    TOP_cmp_le_and_orcm,
		    TOP_cmp_le_andcm,
		    TOP_cmp_le_or,
		    TOP_cmp_le_or_andcm,
		    TOP_cmp_le_orcm,
		    TOP_cmp_le_unc,
		    TOP_cmp_leu,
		    TOP_cmp_leu_unc,
		    TOP_cmp_lt,
		    TOP_cmp_lt_and,
		    TOP_cmp_lt_and_orcm,
		    TOP_cmp_lt_andcm,
		    TOP_cmp_lt_or,
		    TOP_cmp_lt_or_andcm,
		    TOP_cmp_lt_orcm,
		    TOP_cmp_lt_unc,
		    TOP_cmp_ltu,
		    TOP_cmp_ltu_unc,
		    TOP_cmp_ne,
		    TOP_cmp_ne_and,
		    TOP_cmp_ne_and_orcm,
		    TOP_cmp_ne_andcm,
		    TOP_cmp_ne_or,
		    TOP_cmp_ne_or_andcm,
		    TOP_cmp_ne_orcm,
		    TOP_cmp_ne_unc,
		    TOP_cmp_z1_ge_and,
		    TOP_cmp_z1_ge_and_orcm,
		    TOP_cmp_z1_ge_andcm,
		    TOP_cmp_z1_ge_or,
		    TOP_cmp_z1_ge_or_andcm,
		    TOP_cmp_z1_ge_orcm,
		    TOP_cmp_z1_gt_and,
		    TOP_cmp_z1_gt_and_orcm,
		    TOP_cmp_z1_gt_andcm,
		    TOP_cmp_z1_gt_or,
		    TOP_cmp_z1_gt_or_andcm,
		    TOP_cmp_z1_gt_orcm,
		    TOP_cmp_z1_le_and,
		    TOP_cmp_z1_le_and_orcm,
		    TOP_cmp_z1_le_andcm,
		    TOP_cmp_z1_le_or,
		    TOP_cmp_z1_le_or_andcm,
		    TOP_cmp_z1_le_orcm,
		    TOP_cmp_z1_lt_and,
		    TOP_cmp_z1_lt_and_orcm,
		    TOP_cmp_z1_lt_andcm,
		    TOP_cmp_z1_lt_or,
		    TOP_cmp_z1_lt_or_andcm,
		    TOP_cmp_z1_lt_orcm,
		    TOP_cmp_z2_ge_and,
		    TOP_cmp_z2_ge_and_orcm,
		    TOP_cmp_z2_ge_andcm,
		    TOP_cmp_z2_ge_or,
		    TOP_cmp_z2_ge_or_andcm,
		    TOP_cmp_z2_ge_orcm,
		    TOP_cmp_z2_gt_and,
		    TOP_cmp_z2_gt_and_orcm,
		    TOP_cmp_z2_gt_andcm,
		    TOP_cmp_z2_gt_or,
		    TOP_cmp_z2_gt_or_andcm,
		    TOP_cmp_z2_gt_orcm,
		    TOP_cmp_z2_le_and,
		    TOP_cmp_z2_le_and_orcm,
		    TOP_cmp_z2_le_andcm,
		    TOP_cmp_z2_le_or,
		    TOP_cmp_z2_le_or_andcm,
		    TOP_cmp_z2_le_orcm,
		    TOP_cmp_z2_lt_and,
		    TOP_cmp_z2_lt_and_orcm,
		    TOP_cmp_z2_lt_andcm,
		    TOP_cmp_z2_lt_or,
		    TOP_cmp_z2_lt_or_andcm,
		    TOP_cmp_z2_lt_orcm,
		    TOP_cmp4_eq,
		    TOP_cmp4_eq_and,
		    TOP_cmp4_eq_and_orcm,
		    TOP_cmp4_eq_andcm,
		    TOP_cmp4_eq_or,
		    TOP_cmp4_eq_or_andcm,
		    TOP_cmp4_eq_orcm,
		    TOP_cmp4_eq_unc,
		    TOP_cmp4_ge,
		    TOP_cmp4_ge_and,
		    TOP_cmp4_ge_and_orcm,
		    TOP_cmp4_ge_andcm,
		    TOP_cmp4_ge_or,
		    TOP_cmp4_ge_or_andcm,
		    TOP_cmp4_ge_orcm,
		    TOP_cmp4_ge_unc,
		    TOP_cmp4_geu,
		    TOP_cmp4_geu_unc,
		    TOP_cmp4_gt,
		    TOP_cmp4_gt_and,
		    TOP_cmp4_gt_and_orcm,
		    TOP_cmp4_gt_andcm,
		    TOP_cmp4_gt_or,
		    TOP_cmp4_gt_or_andcm,
		    TOP_cmp4_gt_orcm,
		    TOP_cmp4_gt_unc,
		    TOP_cmp4_gtu,
		    TOP_cmp4_gtu_unc,
		    TOP_cmp4_i_eq,
		    TOP_cmp4_i_eq_and,
		    TOP_cmp4_i_eq_and_orcm,
		    TOP_cmp4_i_eq_andcm,
		    TOP_cmp4_i_eq_or,
		    TOP_cmp4_i_eq_or_andcm,
		    TOP_cmp4_i_eq_orcm,
		    TOP_cmp4_i_eq_unc,
		    TOP_cmp4_i_ge,
		    TOP_cmp4_i_ge_unc,
		    TOP_cmp4_i_geu,
		    TOP_cmp4_i_geu_unc,
		    TOP_cmp4_i_gt,
		    TOP_cmp4_i_gt_unc,
		    TOP_cmp4_i_gtu,
		    TOP_cmp4_i_gtu_unc,
		    TOP_cmp4_i_le,
		    TOP_cmp4_i_le_unc,
		    TOP_cmp4_i_leu,
		    TOP_cmp4_i_leu_unc,
		    TOP_cmp4_i_lt,
		    TOP_cmp4_i_lt_unc,
		    TOP_cmp4_i_ltu,
		    TOP_cmp4_i_ltu_unc,
		    TOP_cmp4_i_ne,
		    TOP_cmp4_i_ne_and,
		    TOP_cmp4_i_ne_and_orcm,
		    TOP_cmp4_i_ne_andcm,
		    TOP_cmp4_i_ne_or,
		    TOP_cmp4_i_ne_or_andcm,
		    TOP_cmp4_i_ne_orcm,
		    TOP_cmp4_i_ne_unc,
		    TOP_cmp4_le,
		    TOP_cmp4_le_and,
		    TOP_cmp4_le_and_orcm,
		    TOP_cmp4_le_andcm,
		    TOP_cmp4_le_or,
		    TOP_cmp4_le_or_andcm,
		    TOP_cmp4_le_orcm,
		    TOP_cmp4_le_unc,
		    TOP_cmp4_leu,
		    TOP_cmp4_leu_unc,
		    TOP_cmp4_lt,
		    TOP_cmp4_lt_and,
		    TOP_cmp4_lt_and_orcm,
		    TOP_cmp4_lt_andcm,
		    TOP_cmp4_lt_or,
		    TOP_cmp4_lt_or_andcm,
		    TOP_cmp4_lt_orcm,
		    TOP_cmp4_lt_unc,
		    TOP_cmp4_ltu,
		    TOP_cmp4_ltu_unc,
		    TOP_cmp4_ne,
		    TOP_cmp4_ne_and,
		    TOP_cmp4_ne_and_orcm,
		    TOP_cmp4_ne_andcm,
		    TOP_cmp4_ne_or,
		    TOP_cmp4_ne_or_andcm,
		    TOP_cmp4_ne_orcm,
		    TOP_cmp4_ne_unc,
		    TOP_cmp4_z1_ge_and,
		    TOP_cmp4_z1_ge_and_orcm,
		    TOP_cmp4_z1_ge_andcm,
		    TOP_cmp4_z1_ge_or,
		    TOP_cmp4_z1_ge_or_andcm,
		    TOP_cmp4_z1_ge_orcm,
		    TOP_cmp4_z1_gt_and,
		    TOP_cmp4_z1_gt_and_orcm,
		    TOP_cmp4_z1_gt_andcm,
		    TOP_cmp4_z1_gt_or,
		    TOP_cmp4_z1_gt_or_andcm,
		    TOP_cmp4_z1_gt_orcm,
		    TOP_cmp4_z1_le_and,
		    TOP_cmp4_z1_le_and_orcm,
		    TOP_cmp4_z1_le_andcm,
		    TOP_cmp4_z1_le_or,
		    TOP_cmp4_z1_le_or_andcm,
		    TOP_cmp4_z1_le_orcm,
		    TOP_cmp4_z1_lt_and,
		    TOP_cmp4_z1_lt_and_orcm,
		    TOP_cmp4_z1_lt_andcm,
		    TOP_cmp4_z1_lt_or,
		    TOP_cmp4_z1_lt_or_andcm,
		    TOP_cmp4_z1_lt_orcm,
		    TOP_cmp4_z2_ge_and,
		    TOP_cmp4_z2_ge_and_orcm,
		    TOP_cmp4_z2_ge_andcm,
		    TOP_cmp4_z2_ge_or,
		    TOP_cmp4_z2_ge_or_andcm,
		    TOP_cmp4_z2_ge_orcm,
		    TOP_cmp4_z2_gt_and,
		    TOP_cmp4_z2_gt_and_orcm,
		    TOP_cmp4_z2_gt_andcm,
		    TOP_cmp4_z2_gt_or,
		    TOP_cmp4_z2_gt_or_andcm,
		    TOP_cmp4_z2_gt_orcm,
		    TOP_cmp4_z2_le_and,
		    TOP_cmp4_z2_le_and_orcm,
		    TOP_cmp4_z2_le_andcm,
		    TOP_cmp4_z2_le_or,
		    TOP_cmp4_z2_le_or_andcm,
		    TOP_cmp4_z2_le_orcm,
		    TOP_cmp4_z2_lt_and,
		    TOP_cmp4_z2_lt_and_orcm,
		    TOP_cmp4_z2_lt_andcm,
		    TOP_cmp4_z2_lt_or,
		    TOP_cmp4_z2_lt_or_andcm,
		    TOP_cmp4_z2_lt_orcm,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// 0 to dependent BR
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the ILOG functional unit class:
  // A-type instructions which can execute in any I or M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("ILOG",
		    TOP_and,
		    TOP_and_i,
		    TOP_andcm,
		    TOP_andcm_i,
		    TOP_or,
		    TOP_or_i,
		    TOP_xor,
		    TOP_xor_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// +1 to dependent LD/ST base reg
					// +2 to dependent MMMUL, MMSHF, MMALU
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the ISHF functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("ISHF",
		    TOP_dep,
		    TOP_dep_i,
		    TOP_dep_i_z,
		    TOP_dep_z,
		    TOP_extr,
		    TOP_extr_u,
		    TOP_shrp,
		    TOP_shl_i,		// pseudo of dep_z
		    TOP_shr_i,		// pseudo of extr
		    TOP_shr_i_u,	// pseudo of extr_u
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the LD functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("LD",
		    TOP_ld1,
		    TOP_ld1_i,
		    TOP_ld1_r,
		    TOP_ld2,
		    TOP_ld2_i,
		    TOP_ld2_r,
		    TOP_ld4,
		    TOP_ld4_i,
		    TOP_ld4_r,
		    TOP_ld8,
		    TOP_ld8_i,
		    TOP_ld8_r,
		    TOP_ld8_fill,
		    TOP_ld8_i_fill,
		    TOP_ld8_r_fill,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +1 to dependent LD/ST base reg
					// +1 to dependent MM operation
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(2);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the LFETCH functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("LFETCH",
		    TOP_lfetch,
		    TOP_lfetch_excl,
		    TOP_lfetch_fault,
		    TOP_lfetch_fault_excl,
		    TOP_lfetch_i,
		    TOP_lfetch_i_excl,
		    TOP_lfetch_i_fault,
		    TOP_lfetch_i_fault_excl,
		    TOP_lfetch_r,
		    TOP_lfetch_r_excl,
		    TOP_lfetch_r_fault,
		    TOP_lfetch_r_fault_excl,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(6);			// assume L2 latency

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the LONG_I functional unit class:
  // L-type instructions which can execute in any I unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("LONG_I",
		    TOP_movl,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the MMALU_A functional unit class:
  // A-type instructions which can execute in any I or M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMALU_A",
		    TOP_padd1,
		    TOP_padd2,
		    TOP_padd1_sss,
		    TOP_padd2_sss,
		    TOP_padd1_uuu,
		    TOP_padd2_uuu,
		    TOP_padd1_uus,
		    TOP_padd2_uus,
		    TOP_padd4,
		    TOP_pavg1,
		    TOP_pavg1_raz,
		    TOP_pavg2,
		    TOP_pavg2_raz,
		    TOP_pavgsub1,
		    TOP_pavgsub2,
		    TOP_pcmp1_eq,
		    TOP_pcmp2_eq,
		    TOP_pcmp4_eq,
		    TOP_pcmp1_gt,
		    TOP_pcmp2_gt,
		    TOP_pcmp4_gt,
		    TOP_pshladd2,
		    TOP_pshradd2,
		    TOP_psub1,
		    TOP_psub2,
		    TOP_psub1_sss,
		    TOP_psub2_sss,
		    TOP_psub1_uuu,
		    TOP_psub2_uuu,
		    TOP_psub1_uus,
		    TOP_psub2_uus,
		    TOP_psub4,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +4 (or +10) to dependent IALU,
					// ILOG, ISHF, ST, LD
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the MMALU_I functional unit class:
  // I-type instructions which can execute in any I unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMALU_I",
		    TOP_pmax1_u,
		    TOP_pmax2,
		    TOP_pmin1_u,
		    TOP_pmin2,
		    TOP_psad1,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +4 (or +10) to dependent IALU,
					// ILOG, ISHF, ST, LD
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the MMMUL functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMMUL",
		    TOP_pmpy2_l,
		    TOP_pmpy2_r,
		    TOP_pmpyshr2,
		    TOP_pmpyshr2_u,
		    TOP_popcnt,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +4 (or +10) to dependent IALU,
					// ILOG, ISHF, ST, LD
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the MMSHF functional unit class:
  // I-type instructions which can execute in any I unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMSHF",
		    TOP_mix1_l,
		    TOP_mix1_r,
		    TOP_mix2_l,
		    TOP_mix2_r,
		    TOP_mix4_l,
		    TOP_mix4_r,
		    TOP_mux1,
		    TOP_mux2,
		    TOP_pack2_sss,
		    TOP_pack2_uss,
		    TOP_pack4_sss,
		    TOP_pshl2,
		    TOP_pshl2_i,
		    TOP_pshl4,
		    TOP_pshl4_i,
		    TOP_pshr2,
		    TOP_pshr2_u,
		    TOP_pshr2_i,
		    TOP_pshr2_i_u,
		    TOP_pshr4,
		    TOP_pshr4_u,
		    TOP_pshr4_i,
		    TOP_pshr4_i_u,
		    TOP_shl,
		    TOP_shr,
		    TOP_shr_u,
		    TOP_unpack1_h,
		    TOP_unpack1_l,
		    TOP_unpack2_h,
		    TOP_unpack2_l,
		    TOP_unpack4_h,
		    TOP_unpack4_l,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);		// +4 (or +10) to dependent IALU,
					// ILOG, ISHF, ST, LD
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the NOP_B functional unit class:
  // B-type instructions which can execute in any B unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("NOP_B",
		    TOP_break_b,
		    TOP_nop_b,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the NOP_I functional unit class:
  // I-type instructions which can execute in any I unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("NOP_I",
		    TOP_break_i,
		    TOP_nop_i,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the NOP_M functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("NOP_M",
		    TOP_break_m,
		    TOP_nop_m,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the NOP_F functional unit class:
  // F-type instructions which can execute in any F unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("NOP_F",
		    TOP_break_f,
		    TOP_nop_f,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the NOP_X functional unit class:
  // X-type instructions which can execute in any I unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("NOP_X",
		    TOP_break_x,
		    TOP_nop_x,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the PNT functional unit class:
  // A-type instructions which can execute in any I or M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("PNT",
		    TOP_addp4,
		    TOP_addp4_i,
		    TOP_shladdp4,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the RSE_B functional unit class:
  // B-type instructions which can execute in any B unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("RSE_B",
		    TOP_clrrrb,
		    TOP_clrrrb_pr,
		    TOP_cover,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the RSE_M functional unit class:
  // M-type instructions which can only execute in M0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("RSE_M",
		    TOP_flushrs,
		    TOP_loadrs,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SEM functional unit class:
  // M-type instructions which can only execute in M0.
  // These instructions are not pipelined and on Itanium (tm)
  // cause the pipeline to stall.
  //
  // NOTE: We don't model the pipeline stall here since that is
  // effectively handled by these being barriers. We do however
  // model that this inst is not pipelined.
  //
  // NOTE: The latency is spec-ed as the latency of L2, L3 or
  // memory + 5 clocks. We assume L2 latency here.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SEM",
		    TOP_cmpxchg1,
		    TOP_cmpxchg2,
		    TOP_cmpxchg4,
		    TOP_cmpxchg8,
		    TOP_fetchadd4,
		    TOP_fetchadd8,
		    TOP_xchg1,
		    TOP_xchg2,
		    TOP_xchg4,
		    TOP_xchg8,
		    TOP_UNDEFINED);
  {
    int i;
    const int L2 = 6;
    const int latency = L2 + 5;
    Any_Operand_Access_Time(0);
    Any_Result_Available_Time(latency);
    Resource_Requirement(res_issue, 0);
    Resource_Requirement(res_int_or_mem, 0);
    Resource_Requirement(res_memory, 0);
    Resource_Requirement(res_M0, 0);	// can only execute in M0
    for (i = 0; i < latency; ++i) Resource_Requirement(res_sem, i);
    Load_Access_Time(latency);
  }

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SFCVTFX functional unit class:
  // F-type instructions which can execute in any F unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SFCVTFX",
		    TOP_fpcvt_fx,
		    TOP_fpcvt_fx_trunc,
		    TOP_fpcvt_fxu,
		    TOP_fpcvt_fxu_trunc,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);		// +2 to dependent Fxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SFMAC functional unit class:
  // F-type instructions which can execute in any F unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SFMAC",
		    TOP_fpma,
		    TOP_fpms,
		    TOP_fpnma,
		    TOP_fpmpy,		// pseudo of fpma
		    TOP_fpnmpy,		// pseudo of fpnma
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);		// +2 to dependent SFMISC
					// +2 to dependent Fxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SFMERGESE functional unit class:
  // F-type instructions which can only execute in F0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SFMERGESE",
		    TOP_fpmerge_se,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);		// +2 to dependent Fxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0); 	// can only execute in F0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SFMISC functional unit class:
  // F-type instructions which can only execute in F0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SFMISC",
		    TOP_fpamax,
		    TOP_fpamin,
		    TOP_fpcmp_eq,
		    TOP_fpcmp_ge,
		    TOP_fpcmp_gt,
		    TOP_fpcmp_le,
		    TOP_fpcmp_lt,
		    TOP_fpcmp_neq,
		    TOP_fpcmp_nge,
		    TOP_fpcmp_ngt,
		    TOP_fpcmp_nle,
		    TOP_fpcmp_nlt,
		    TOP_fpcmp_ord,
		    TOP_fpcmp_unord,
		    TOP_fpmax,
		    TOP_fpmerge_ns,
		    TOP_fpmerge_s,
		    TOP_fpmin,
		    TOP_fprcpa,
		    TOP_fprsqrta,
		    TOP_fpneg,		// pseudo of fpmerge_ns
		    TOP_fpnegabs,	// pseudo of fpmrege_ns
		    TOP_fpabs,		// pseudo of fpmerge_s
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);		// +2 to dependent Fxxx
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0); 	// can only execute in F0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the STF functional unit class:
  // M-type instructions which can execute any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("STF",
		    TOP_stf8,
		    TOP_stf8_i,
		    TOP_stfd,
		    TOP_stfd_i,
		    TOP_stfe,
		    TOP_stfe_i,
		    TOP_stfs,
		    TOP_stfs_i,
		    TOP_stf_spill,
		    TOP_stf_i_spill,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Store_Available_Time(9);		// ??? not sure

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the ST functional unit class:
  // M-type instructions which can execute any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("ST",
		    TOP_st1,
		    TOP_st1_i,
		    TOP_st2,
		    TOP_st2_i,
		    TOP_st4,
		    TOP_st4_i,
		    TOP_st8,
		    TOP_st8_i,
		    TOP_st8_spill,
		    TOP_st8_i_spill,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Store_Available_Time(2);		// ??? not sure

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SYST_B2 functional unit class:
  // B-type instructions which can execute in B0 or B2.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SYST_B2",
		    TOP_bsw_0,
		    TOP_bsw_1,
		    TOP_rfi,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);
  Resource_Requirement(res_B0_or_B2, 0); // can only execute in B0 or B2

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SYST_B functional unit class:
  // B-type instructions which can execute in B0 or B1.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SYST_B",
		    TOP_epc,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);
  Resource_Requirement(res_B0_or_B1, 0); // can only execute in B0 or B1

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SYST_M0 functional unit class:
  // M-type instructions which can only execute in M0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SYST_M0_0",
		    TOP_mf_a,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_1",
		    TOP_alloc,
		    TOP_alloc_3,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_2",
		    TOP_fc,
		    TOP_itc_d,
		    TOP_itc_i,
		    TOP_itr_d,
		    TOP_itr_i,
		    TOP_probe_i_r,
		    TOP_probe_i_w,
		    TOP_probe_r,
		    TOP_probe_r_fault,
		    TOP_probe_rw_fault,
		    TOP_probe_w,
		    TOP_probe_w_fault,
		    TOP_ptc_e,
		    TOP_ptc_g,
		    TOP_ptc_ga,
		    TOP_ptc_l,
		    TOP_ptr_d,
		    TOP_ptr_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// ??? really is 'variable'
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_3",
		    TOP_rum,
		    TOP_sum,
		    TOP_mov_t_psrum,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_4",
		    TOP_rsm,
		    TOP_ssm,
		    TOP_mov_t_psr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(5);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_5",
		    TOP_tak,
		    TOP_tpa,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(6);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_6",
		    TOP_mov_t_rr,
		    TOP_mov_t_pkr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(10);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_7",
		    TOP_thash,
		    TOP_ttag,
		    TOP_mov_f_psrum,
		    TOP_mov_f_psr,
		    TOP_mov_f_rr,
		    TOP_mov_f_pkr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(13);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_8",
		    TOP_mov_t_msr,
		    TOP_mov_t_pmd,
		    TOP_mov_t_pmc,
		    TOP_mov_t_ibr,
		    TOP_mov_t_dbr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(35);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  Instruction_Group("SYST_M0_9",
		    TOP_mov_f_msr,
		    TOP_mov_f_pmd,
		    TOP_mov_f_pmc,
		    TOP_mov_f_ibr,
		    TOP_mov_f_dbr,
		    TOP_mov_f_cpuid,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(38);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the SYST_M functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SYST_M",
		    TOP_fwb,
		    TOP_invala,
		    TOP_invala_e,
		    TOP_invala_f_e,
		    TOP_mf,
		    TOP_srlz_d,
		    TOP_srlz_i,
		    TOP_sync_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the TBIT functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TBIT",
		    TOP_tbit_nz,
		    TOP_tbit_nz_and,
		    TOP_tbit_nz_or,
		    TOP_tbit_nz_or_andcm,
		    TOP_tbit_nz_unc,
		    TOP_tbit_z,
		    TOP_tbit_z_and,
		    TOP_tbit_z_or,
		    TOP_tbit_z_or_andcm,
		    TOP_tbit_z_unc,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// 0 to dependent BR
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the TOAR_I functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TOAR_I",
		    TOP_mov_t_ar_r_i,
		    TOP_mov_t_ar_i_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// depends on dest register
					// 0 to dependent BR (pfs only)
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the TOAR_M functional unit class:
  // M-type instructions which can only execute in M0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TOAR_M",
		    TOP_mov_t_ar_r_m,
		    TOP_mov_t_ar_i_m,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// depends on dest register
					// 0 to dependent BR (pfs only)
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the TOBR functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TOBR",
		    TOP_mov_t_br,
		    TOP_mov_t_br_i,
		    TOP_mov_t_br_ret,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// 0 to dependent BR
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the TOCR functional unit class:
  // M-type instructions which can only execute in M0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TOCR",
		    TOP_mov_t_cr,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// depends on dest register
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);	// can only execute in M0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the TOFR functional unit class:
  // M-type instructions which can execute in any M unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TOFR",
		    TOP_setf_exp,
		    TOP_setf_d,
		    TOP_setf_s,
		    TOP_setf_sig,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the TOPR functional unit class:
  // I-type instructions which can only execute in I0.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TOPR",
		    TOP_mov_t_pr,
		    TOP_mov_t_pr_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// 0 to dependent BR
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the XMA functional unit class:
  // F-type instructions which can execute in any F unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("XMA",
		    TOP_xma_h,
		    TOP_xma_hu,
		    TOP_xma_l,
		    TOP_xma_lu,		// pseudo of xma_l
		    TOP_xmpy_h,		// pseudo of xma_h
		    TOP_xmpy_hu,	// pseudo of xma_hu
		    TOP_xmpy_l,		// pseudo of xma_l
		    TOP_xmpy_lu,	// pseudo of xma_l
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(7);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to the XTD functional unit class:
  // I-type instructions which can execute in any I unit.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("XTD",
		    TOP_czx1_l,
		    TOP_czx1_r,
		    TOP_czx2_l,
		    TOP_czx2_r,
		    TOP_sxt1,
		    TOP_sxt2,
		    TOP_sxt4,
		    TOP_zxt1,
		    TOP_zxt2,
		    TOP_zxt4,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);

  ////////////////////////////////////////////////////
  //
  // UNKNOWN_1
  // I-type instructions which can...
  //
  ////////////////////////////////////////////////////
  Instruction_Group("UNKNOWN_1",
		    TOP_tnat_z,
		    TOP_tnat_z_unc,
		    TOP_tnat_z_and,
		    TOP_tnat_nz_and,
		    TOP_tnat_z_or,
		    TOP_tnat_nz_or,
		    TOP_tnat_z_or_andcm,
		    TOP_tnat_nz_or_andcm,
		    TOP_tnat_nz,
		    TOP_tnat_nz_unc,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);		// ??? not sure
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_int_or_mem, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);	// can only execute in I0

  ////////////////////////////////////////////////////
  //
  // UNKNOWN_2
  //
  ////////////////////////////////////////////////////
  Instruction_Group("dummy",
		    TOP_break,
		    TOP_nop,
		    TOP_mov_f_ar,
		    TOP_mov_t_ar_r,
		    TOP_mov_t_ar_i,
		    TOP_chk_s,
		    TOP_asm,
		    TOP_intrncall,
		    TOP_spadjust,
		    TOP_copy_br,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);

  Machine_Done("itanium.c");
}

