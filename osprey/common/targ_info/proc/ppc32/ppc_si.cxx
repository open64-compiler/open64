/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.

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

*/

/*PPC32 processor scheduling information
  Generate a scheduling description of a PPC32 processor via the si_gen interface.
*/

#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_IBox, res_EX, res_LS, res_FP; /* MDMX units */
static RESOURCE res_EX0, res_EX1, res_LS0, res_LS1, res_FP0, res_FP1;

void Generate_PowerPC5 (void)
{
  Machine("powerpc5", ISA_SUBSET_POWERPC5);
  res_IBox = RESOURCE_Create ("IBox", 4);
  res_EX = RESOURCE_Create ("Execution", 2);
  res_LS = RESOURCE_Create ("LoadStore", 2);
  res_FP = RESOURCE_Create ("Float", 2);
  res_EX0 = RESOURCE_Create ("Execution", 1);
  res_LS0 = RESOURCE_Create ("LoadStore", 1);
  res_FP0 = RESOURCE_Create ("Float", 1);
  res_EX1 = RESOURCE_Create ("Execution", 1);
  res_LS1 = RESOURCE_Create ("LoadStore", 1);
  res_FP1 = RESOURCE_Create ("Float", 1);


  /* instruction categories and units: Table 19 summarizes
   *
   * However, our model only allows an instruction to be in one
   * instruction group.
   *
   * (X) marks places where we leave off a unit because there
   * are multiple types of units involved (e.g. E0,E1 and LS1)
   *
   * (Y) marks places where we give too broad of a restriction
   * because only one of the pair can execute an instruction
   *
   * 1) Integer ADDs, SUBs, Logical Ops E0, E1, LS1(X) (different pipe)
   * 2) Shifts E0, E1
   * 3) LUI E0, E1, LS1(X)
   * 4) Branches/Jumps E0 (Y)
   * 5) CP1 Branches E0 (Y)
   * 6) Sets E0 (Y)
   * 7) Traps E0 (Y)
   * 8) CLZ/CLO E0 (Y)
   * 9) Conditional Moves E0, E1
   * 10) Integer Multiply/Divide E1 (note unusual latency) (Y)
   * 11) MT/MF HI/LO E1 (Y)
   * 12) MOVT, MOVF E0, E1
   * 13) MOVZ, MOVN E0, E1
   *
   * 14) Integer Loads, Stores, FP Loads, Stores LS0 LS1 (same pipeline)
   * 15) Indexed Loads, Stores LS1 (Y)
   * 16) TLB OPs LS1 (Y)
   * 17) MT/MF CP0 LS1 (Y)
   * 18) Cache Ops LS1 (Y)
   *
   * 19) Normal FP ops FP0, FP1
   * 20) C.cond FP0 (Y)
   * 21) CABS FP0 (this is a MIPS-3D instruction) (Y)
   * "The majority of MIPS-3D instructions can be issued to FP1 pipe only" (Y)
   */

  /* For this instruction group, ALU ops can co-issue, so let's pretend
     that the latency is zero. */

  Instruction_Group ("Load/Store", 
			  TOP_lbz, /* type 14, lat 0 */
			  TOP_lbzu, /* type 14, lat 0 */
			  TOP_lhz, /* type 14, lat 0 */
			  TOP_lhzu, /* type 14, lat 0 */
			  TOP_lha, /* type 14, lat 0 */
			  TOP_lhau, /* type 14, lat 0 */			  
			  TOP_lwz, /* type 14, lat 0 */
			  TOP_lwzu, /* type 14, lat 0 */
			  TOP_stb, /* type 14, lat 0 */
			  TOP_sth, /* type 14, lat 0 */
			  TOP_stw, /* type 14, lat 0 */
			  TOP_stbu, /* type 14, lat 0 */
			  TOP_sthu, /* type 14, lat 0 */
			  TOP_stwu, /* type 14, lat 0 */
			  
        TOP_lbzx,
      	TOP_lbzux,
      	TOP_lhzx,
      	TOP_lhzux,
      	TOP_lhax,
      	TOP_lhaux,
      	TOP_lwzx,
      	TOP_lwzux,
        	
      	TOP_stbx,
      	TOP_stbux,
      	TOP_sthx,
      	TOP_sthux,
      	TOP_stwx,
      	TOP_stwux,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_LS, 0);

		    /* Table 4: CPU (integer) Arithmetic Instructions */

  Instruction_Group ("Integer ALU group 1",
			  TOP_add, /* type 1, lat 1 */
			  TOP_addi, /* type 1, lat 1 */
			  TOP_addis,
        TOP_add_, /* type 1, lat 1 */
			  TOP_addic, /* type 1, lat 1 */
			  TOP_addc,
				TOP_addco,
				TOP_adde,
				TOP_addeo,
				TOP_addme,
				TOP_addmeo,
				TOP_addze,
				TOP_addzeo,
				TOP_la,
			  TOP_li,
			  TOP_lis,
			  TOP_mr,
			  TOP_subf, /* type 1, lat 1 */
			  TOP_subfc,
			  TOP_subfe,
			  TOP_subfic,
			  TOP_subfco,
				TOP_subfeo,
				TOP_subfme,
				TOP_subfmeo,
				TOP_subfze,
				TOP_subfzeo,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);

  Instruction_Group ("Integer ALU group 1a",
			  TOP_cmpw, /* type 6, lat 1 */
			  TOP_cmpwi, /* type 6, lat 1 */
			  TOP_cmplwi, /* type 6, lat 1 */
			  TOP_cmplw, /* type 6, lat 1 */
			  TOP_neg,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

  Instruction_Group ("Integer ALU group 2",
			  TOP_divw, /* type 10, lat 36 */ /* not pipelined */
			  TOP_divwo,
              TOP_divwu,
              TOP_divwuo,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (36);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("Integer ALU group 3",
			  TOP_mullw, /* type 10, lat 3 */
			  TOP_mulli,
              TOP_mullwo,
              TOP_mulhw,
              TOP_mulhwu,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (3);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("Integer ALU group 4",
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (68);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("Integer ALU group 4",
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

		    /* Table 5: CPU Logical Instructions */

  Instruction_Group ("CPU Logical",
			  TOP_and, /* type 1, lat 1 */
			  TOP_andc, 
			  TOP_andi_,
			  TOP_nor, /* type 1, lat 1 */
			  TOP_or, /* type 1, lat 1 */
			  TOP_ori, /* type 1, lat 1 */
			  TOP_xor, /* type 1, lat 1 */
			  TOP_xori, /* type 1, lat 1 */
			  TOP_andis_,
              TOP_oris,
              TOP_xoris,
              TOP_not,
              TOP_nand,
              TOP_eqv,
              TOP_orc,
              TOP_extsb,
              TOP_extsh,
              TOP_crand,
                           TOP_cror,
                           TOP_crxor,
                           TOP_crnand,
                           TOP_crnor,
                           TOP_creqv,
                           TOP_crandc,
                           TOP_crorc,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);

		    /* Table 6: CPU Move Instructions */

  Instruction_Group ("CPU move 1",
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX1, 0);

  Instruction_Group ("CPU move 2",
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);

		    /* Table 7: CPU Shift Instructions */

  Instruction_Group ("CPU Shift",
			  TOP_slwi, /* type 2, lat 1 */
			  TOP_slw, /* type 2, lat 1 */
			  TOP_srawi, /* type 2, lat 1 */
			  TOP_sraw, /* type 2, lat 1 */
			  TOP_srwi, /* type 2, lat 1 */
			  TOP_srw, /* type 2, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);


  Instruction_Group ("CPU Left Rotate",
			  TOP_rlwinm,
              TOP_rlwimi,
              TOP_rlwnm,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement(res_IBox, 0);
  Resource_Requirement(res_EX, 0);
  


		    /* Table 8: CPU Branch and Jump Instructions */

  Instruction_Group ("CPU Branch/Jump",
			  // reletive branch
        	TOP_beq,  
        	TOP_bge,
        	TOP_bgt,
        	TOP_ble,
        	TOP_blt,
        	
        	TOP_bnl,
        	TOP_bne,
        	TOP_bng,
        	
        	TOP_bso,
        	TOP_bns,
        	TOP_bun,
        	TOP_bnu,
        	
        	// absoult branch
        	TOP_beqa,  
        	TOP_bgea,
        	TOP_bgta,
        	TOP_blea,
        	TOP_blta,
        	
        	TOP_bnla,
        	TOP_bnea,
        	TOP_bnga,
        	
        	TOP_bsoa,
        	TOP_bnsa,
        	TOP_buna,
        	TOP_bnua,
        	
        	// branch to lr
        	TOP_beqlr,  
        	TOP_bgelr,
        	TOP_bgtlr,
        	TOP_blelr,
        	TOP_bltlr,
        	
        	TOP_bnllr,
        	TOP_bnelr,
        	TOP_bnglr,
        	
        	TOP_bsolr,
        	TOP_bnslr,
        	TOP_bunlr,
        	TOP_bnulr,
        	
        	// branch to ctr
        	TOP_beqctr,  
        	TOP_bgectr,
        	TOP_bgtctr,
        	TOP_blectr,
        	TOP_bltctr,
        	
        	TOP_bnlctr,
        	TOP_bnectr,
        	TOP_bngctr,
        	
        	TOP_bsoctr,
        	TOP_bnsctr,
        	TOP_bunctr,
        	TOP_bnuctr,
        	
        	// reletive branch, update lr
        	TOP_beql,  
        	TOP_bgel,
        	TOP_bgtl,
        	TOP_blel,
        	TOP_bltl,
        	
        	TOP_bnll,
        	TOP_bnel,
        	TOP_bngl,
        	
        	TOP_bsol,
        	TOP_bnsl,
        	TOP_bunl,
        	TOP_bnul,
        	
        	// absoult branch, update lr
        	TOP_beqla,  
        	TOP_bgela,
        	TOP_bgtla,
        	TOP_blela,
        	TOP_bltla,
        	
        	TOP_bnlla,
        	TOP_bnela,
        	TOP_bngla,
        	
        	TOP_bsola,
        	TOP_bnsla,
        	TOP_bunla,
        	TOP_bnula,
        	
        	// branch to lr, update lr
        	TOP_beqlrl,  
        	TOP_bgelrl,
        	TOP_bgtlrl,
        	TOP_blelrl,
        	TOP_bltlrl,
        	
        	TOP_bnllrl,
        	TOP_bnelrl,
        	TOP_bnglrl,
        	
        	TOP_bsolrl,
        	TOP_bnslrl,
        	TOP_bunlrl,
        	TOP_bnulrl,
        	
        	// branch to ctr, update lr
        	TOP_beqctrl,  
        	TOP_bgectrl,
        	TOP_bgtctrl,
        	TOP_blectrl,
        	TOP_bltctrl,
        	
        	TOP_bnlctrl,
        	TOP_bnectrl,
        	TOP_bngctrl,
        	
        	TOP_bsoctrl,
        	TOP_bnsctrl,
        	TOP_bunctrl,
        	TOP_bnuctrl,
			  TOP_ba, /* type 4, lat 1 */
			  TOP_bla, /* type 4, lat 1 */
			  TOP_b, /* type 4, lat 1 */
			  TOP_bl, /* type 4, lat 1 */
			  TOP_blrl, /* type 4, lat 1 */
			  TOP_bctrl,
			  TOP_blr, /* type 4, lat 1 */
			  TOP_bctr,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

		    /* Table 9: CPU Trap Instructions */

  Instruction_Group ("CPU Trap",
			  TOP_sc, /* type 7, lat 1 */
			  TOP_tweq, /* type 7, lat 1 */
			  TOP_tweqi, /* type 7, lat 1 */
			  TOP_twge, /* type 7, lat 1 */
			  TOP_twgei, /* type 7, lat 1 */
			  TOP_twlt, /* type 7, lat 1 */
			  TOP_twlti, /* type 7, lat 1 */
			  TOP_twne, /* type 7, lat 1 */
			  TOP_twnei, /* type 7, lat 1 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

    /* FPU loads/stores, not listed in SB-1 manual, 6-9 MIPS R4000
     and table 3-14 MIPS64 Architecture Manual */

  Instruction_Group ("FPU Load/Store 1",
	        TOP_lfs, /* type 14, lat 0 */
          TOP_lfd, /* type 14, lat 0 */
          TOP_stfs, /* type 14, lat 0 */
          TOP_stfd, /* type 14, lat 0 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (0);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_LS, 0);

  Instruction_Group ("FPU Load/Store 2",
          TOP_lfsx, /* type 15, lat 0 */
          TOP_lfdx, /* type 15, lat 0 */
          TOP_stfsx, /* type 15, lat 0 */
          TOP_stfdx, /* type 15, lat 0 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (0);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_LS, 0);
  Resource_Requirement (res_LS1, 0);

    /* Table 23: FPU Arithmetic Instructions (missing paired single) */

  Instruction_Group ("FPU Arithmatic group 0",
			  TOP_fabs, /* type 19, lat 4 */
			  TOP_fnabs,
        TOP_fadds, /* type 19, lat 4 */
			  TOP_fadd, /* type 19, lat 4 */

        TOP_fmuls, /* type 19, lat 4 */
			  TOP_fmul, /* type 19, lat 4 */
			  TOP_fneg, /* type 19, lat 4 */
        TOP_fsubs, /* type 19, lat 4 */
			  TOP_fsub, /* type 19, lat 4 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (5);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

			  /* List of conditions table 6-13 MIPS R4000 User Manual */

  Instruction_Group ("FPU Arithmatic group 0a",
          TOP_fcmpu,
          TOP_fcmpo,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (5);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);
  Resource_Requirement (res_FP0, 0);

  Instruction_Group ("FPU Arithmatic group 1",
			  TOP_fmadds, /* type 19, lat 8 */
			  TOP_fmadd, /* type 19, lat 8 */
			  TOP_fmsubs, /* type 19, lat 8 */
			  TOP_fmsub, /* type 19, lat 8 */
			  TOP_fnmadds, /* type 19, lat 8 */
			  TOP_fnmadd, /* type 19, lat 8 */
			  TOP_fnmsubs, /* type 19, lat 8 */
			  TOP_fnmsub, /* type 19, lat 8 */
			  TOP_fsel,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (5);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 2",
			  TOP_fdivs, /* type 19, lat 24 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (21);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 3",
			  TOP_fdiv, /* type 19, lat 32 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (35);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 4",
			  TOP_fsqrts, /* type 19, lat 28 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (28);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 5",
			  TOP_fsqrt, /* type 19, lat 40 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (40);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 6",
			  TOP_fres, /* type 19, lat 12 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (12);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 7",
//			  TOP_recip_d, /* type 19, lat 20 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (20);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 8",
//			  TOP_fsqrts, /* type 19, lat 16 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (16);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

  Instruction_Group ("FPU Arithmatic group 8",
			  TOP_frsqrte, /* type 19, lat 28 */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (28);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

		    /* Table 24: FPU Move Instructions */

  Instruction_Group ("FPU Move From",
			  TOP_mcrfs, /* type 19, lat 1 */
			  TOP_mflr, 
			  TOP_mcrf,
			  TOP_mfctr,
			  TOP_mfcr,
			  TOP_mtfsb0,
				TOP_mtfsb1,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0); /* ??? */

  Instruction_Group ("FPU Move To",
			  TOP_mtfsf, /* type 19, lat 4 */
			  TOP_mtlr, 
			  TOP_mtctr,
			  TOP_mffs,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0); /* ??? */

  Instruction_Group ("FPU Move/Convert",
			  TOP_fmr, /* type 19, lat 4 */

			  /* Table 25: FPU Convert Instructions */

			  TOP_frsp, /* type 19, lat 4 */
			  TOP_fctiw, /* type 19, lat 4 */
			  TOP_fctiwz, /* type 19, lat 4 */
			  /* 6 paired single instructions skipped here */
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (4);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_FP, 0);

    /* Table 26: FPU Branch Instructions */

  Instruction_Group ("FPU Branch",
			  TOP_UNDEFINED);
  Any_Operand_Access_Time (0);
  Any_Result_Available_Time (1);
  Resource_Requirement (res_IBox, 0);
  Resource_Requirement (res_EX, 0);
  Resource_Requirement (res_EX0, 0);

    /* Table 28: MIPS-3D instructions skipped */

    /* Dummy instructions */

  Instruction_Group ("Dummy",
			  TOP_asm,
			  TOP_intrncall,
			  TOP_spadjust,
			  TOP_simaddi,
			  TOP_begin_pregtn,
			  TOP_end_pregtn,
			  TOP_bwd_bar,
			  TOP_fwd_bar,
			  TOP_label,
			  TOP_nop,
			  TOP_noop,
			  TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);

  Machine_Done();
}
