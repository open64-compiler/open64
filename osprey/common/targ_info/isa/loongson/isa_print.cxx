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

#include <stddef.h>
#include <string.h>
#include "topcode.h"
#include "isa_print_gen.h"

static const char *asmname(TOP topcode)
{
    const char *opcode = TOP_Name(topcode);
    char buf[100];
    int i;
    for( i = 0; i <= strlen(opcode); i++) {
    	if (opcode[i] == '_')
	    {
		buf[i]='\0';
	        break;
	    }
	buf[i] = opcode[i];
    }

    if (strncmp(buf,"mips",4) == 0)
	{
	    for (int j = 0;j <= i-4; j++)
	    buf[j]=buf[j+4];	
	}
    return (strdup(buf));
}


main()
{
  ISA_Print_Begin("loongson");

  Set_AsmName_Func(asmname);

  Define_Macro("END_GROUP", ";;");		// end-of-group marker
  Define_Macro("PREDICATE", "(%s)");		// predicate operand format
  Define_Macro("BEGIN_BUNDLE", "{ %s");		// bundle introducer
  Define_Macro("END_BUNDLE", "}");		// bundle terminator


/*=======================*/
  ISA_PRINT_TYPE print_0;
  print_0 = ISA_Print_Type_Create("print_0", "%5s %s ");
  Operand(0);
  Name();
  Instruction_Print_Group(print_0,
  TOP_nop,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_1;
  print_1 = ISA_Print_Type_Create("print_1", "%5s %s %s,%s,%s");
  Operand(0);
  Name();
  Result(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(print_1,
        TOP_add,
        TOP_addi,
        TOP_addiu,
        TOP_addu,
        TOP_and,
        TOP_andi,
        TOP_dadd,
        TOP_daddi,
        TOP_daddiu,
        TOP_daddu,
        TOP_dsll,
        TOP_dsllv,
        TOP_dsra,
        TOP_dsra32,
        TOP_dsrav,
        TOP_dsrl,
        TOP_dsrl32,
        TOP_dsrlv,
        TOP_dsub,
        TOP_dsubu,
        TOP_nor,
        TOP_or,
        TOP_ori,
        TOP_sll,
        TOP_sllv,
        TOP_slt,
        TOP_slti,
        TOP_sltiu,
        TOP_sltu,
        TOP_sra,
        TOP_srav,
        TOP_srl,
        TOP_srlv,
        TOP_sub,
        TOP_subu,
        TOP_xor,
        TOP_xori,
        TOP_add_s,
        TOP_div_s,
        TOP_mul_s,
        TOP_sub_s,
        TOP_add_d,
        TOP_div_d,
        TOP_mul_d,
        TOP_sub_d,
        TOP_dsll32,
        TOP_madd_d,
        TOP_madd_s,
        TOP_msub_d,
        TOP_msub_s,
        TOP_nmadd_d,
        TOP_nmadd_s,
        TOP_nmsub_d,
        TOP_nmsub_s,
        TOP_movz,
        TOP_movn,
        TOP_divlo,
        TOP_divhi,
        TOP_divulo,
        TOP_divuhi,

        TOP_mult_g,
        TOP_multu_g,
        TOP_dmult_g,
        TOP_dmultu_g,
        TOP_div_g,
        TOP_divu_g,
        TOP_ddiv_g,
        TOP_ddivu_g,
        TOP_mod_g,
        TOP_modu_g,
        TOP_dmod_g,
        TOP_dmodu_g,

	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_2;
  print_2 = ISA_Print_Type_Create("print_2", "%5s %s %s");
  Operand(0);
  Name();
  Operand(1);
  Instruction_Print_Group(print_2,
        TOP_j,
        TOP_jal,
        TOP_jr,
        TOP_mthi,
        TOP_mtlo,
        TOP_break,
        TOP_bc1f,
        TOP_bc1fl,
        TOP_bc1t,
        TOP_bc1tl,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_3;
  print_3 = ISA_Print_Type_Create("print_3", "%5s %s %s,%s(%s)");
  Operand(0);
  Name();
  Result(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(print_3,
        TOP_lb,
        TOP_lbu,
        TOP_ld,
        TOP_ldr,
        TOP_lh,
        TOP_lhu,
        TOP_ll,
        TOP_lld,
        TOP_lw,
        TOP_lwr,
        TOP_lwu,
        TOP_lwc1,
        TOP_ldc1,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_4;
  print_4 = ISA_Print_Type_Create("print_4", "%5s %s %s,%s(%s)");
  Operand(0);
  Name();
  Operand(1);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(print_4,
        TOP_sb,
        TOP_sc,
        TOP_scd,
        TOP_sd,
        TOP_sdl,
        TOP_sdr,
        TOP_sh,
        TOP_sw,
        TOP_swl,
        TOP_swr,
        TOP_swc1,
        TOP_sdc1,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_5;
  print_5 = ISA_Print_Type_Create("print_5", "%5s %s %s,%s,%s");
  Operand(0);
  Name();
  Operand(1);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(print_5,
        TOP_beq,
        TOP_bne,
        TOP_ddiv,
        TOP_ddivu,
        TOP_div,
        TOP_divu,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_6;
  print_6 = ISA_Print_Type_Create("print_6", "%5s %s %s,%s");
  Operand(0);
  Name();
  Operand(1);
  Operand(2);
  Instruction_Print_Group(print_6,
        TOP_bgez,
        TOP_bgezal,
        TOP_bgtz,
        TOP_blez,
        TOP_bltz,
        TOP_bltzal,
        TOP_dmult,
        TOP_dmultu,
        TOP_mult,
        TOP_multu,
        TOP_c_t_s,
        TOP_c_or_s,
        TOP_c_neq_s,
        TOP_c_olg_s,
        TOP_c_uge_s,
        TOP_c_oge_s,
        TOP_c_ugt_s,
        TOP_c_ogt_s,
        TOP_c_st_s,
        TOP_c_gle_s,
        TOP_c_sne_s,
        TOP_c_gl_s,
        TOP_c_nlt_s,
        TOP_c_ge_s,
        TOP_c_nle_s,
        TOP_c_gt_s,
        TOP_c_f_s,
        TOP_c_un_s,
        TOP_c_eq_s,
        TOP_c_ueq_s,
        TOP_c_olt_s,
        TOP_c_ult_s,
        TOP_c_ole_s,
        TOP_c_ule_s,
        TOP_c_sf_s,
        TOP_c_ngle_s,
        TOP_c_seq_s,
        TOP_c_ngl_s,
        TOP_c_lt_s,
        TOP_c_nge_s,
        TOP_c_le_s,
        TOP_c_ngt_s,
        TOP_c_t_d,
        TOP_c_or_d,
        TOP_c_neq_d,
        TOP_c_olg_d,
        TOP_c_uge_d,
        TOP_c_oge_d,
        TOP_c_ugt_d,
        TOP_c_ogt_d,
        TOP_c_st_d,
        TOP_c_gle_d,
        TOP_c_sne_d,
        TOP_c_gl_d,
        TOP_c_nlt_d,
        TOP_c_ge_d,
        TOP_c_nle_d,
        TOP_c_gt_d,
        TOP_c_f_d,
        TOP_c_un_d,
        TOP_c_eq_d,
        TOP_c_ueq_d,
        TOP_c_olt_d,
        TOP_c_ult_d,
        TOP_c_ole_d,
        TOP_c_ule_d,
        TOP_c_sf_d,
        TOP_c_ngle_d,
        TOP_c_seq_d,
        TOP_c_ngl_d,
        TOP_c_lt_d,
        TOP_c_nge_d,
        TOP_c_le_d,
        TOP_c_ngt_d,
        TOP_teq,
        TOP_teqi,
        TOP_tge,
        TOP_tgei,
        TOP_tgeiu,
        TOP_tgeu,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_7;
  print_7 = ISA_Print_Type_Create("print_7", "%5s %s %s,%s");
  Operand(0);
  Name();
  Result(0);
  Operand(1);
  Instruction_Print_Group(print_7,
        TOP_jalr,
        TOP_lui,
        TOP_abs_s,
        TOP_ceil_l_s,
        TOP_ceil_w_s,
        TOP_cfc1,
        TOP_cvt_l_s,
        TOP_cvt_s_w,
        TOP_cvt_s_l,
        TOP_cvt_w_s,
        TOP_dmfc1,
        TOP_floor_l_s,
        TOP_floor_w_s,
        TOP_mfc1,
        TOP_mov_s,
        TOP_neg_s,
        TOP_round_l_s,
        TOP_round_w_s,
        TOP_trunc_l_s,
        TOP_trunc_w_s,
        TOP_abs_d,
        TOP_ceil_l_d,
        TOP_ceil_w_d,
        TOP_cvt_l_d,
        TOP_cvt_s_d,
        TOP_cvt_w_d,
        TOP_floor_l_d,
        TOP_floor_w_d,
        TOP_mov_d,
        TOP_neg_d,
        TOP_round_l_d,
        TOP_round_w_d,
        TOP_trunc_l_d,
        TOP_trunc_w_d,
        TOP_cvt_d_s,
        TOP_cvt_d_w,
        TOP_cvt_d_l,
        TOP_sqrt_s,
        TOP_sqrt_d,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_8;
  print_8 = ISA_Print_Type_Create("print_8", "%5s %s %s");
  Operand(0);
  Name();
  Result(0);
  Instruction_Print_Group(print_8,
        TOP_mfhi,
        TOP_mflo,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_9;
  print_9 = ISA_Print_Type_Create("print_9", "%5s %s %s,%s");
  Operand(0);
  Name();
  Operand(1);
  Result(0);
  Instruction_Print_Group(print_9,
        TOP_ctc1,
        TOP_dmtc1,
        TOP_mtc1,
	TOP_UNDEFINED);

/*=======================*/
  ISA_PRINT_TYPE print_10;
  print_10 = ISA_Print_Type_Create("print_10", "%5s %s %s,%s(%s)");
  Operand(0);
  Name();
  Result(0);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(print_10,
        TOP_ldl,
        TOP_lwl,
	TOP_UNDEFINED);


  ISA_Print_End();
  return 0;
}
