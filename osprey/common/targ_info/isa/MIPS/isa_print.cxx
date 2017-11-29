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
//  $Revision: 1.17 $
//  $Date: 2006/05/30 06:52:26 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa_print.cxx,v $

#include <stddef.h>
#include <string.h>
#include "topcode.h"
#include "isa_print_gen.h"

// Multiple topcodes map to the same assembly name. To disambiguate the 
// topcodes, we append a suffix to the basename. By convention, the 
// suffix starts with an underscore. To get the assembly name we strip off
// the suffix.

main()
{
  ISA_Print_Begin("MIPS");

  Define_Macro("END_GROUP", ";");		// end-of-group marker
  Define_Macro("PREDICATE", "(%s)");		// predicate operand format
  Define_Macro("BEGIN_BUNDLE", "");	// bundle introducer
  Define_Macro("END_BUNDLE", ";");		// bundle terminator

  Set_AsmName_Func(NULL);
#ifdef TARG_SL
   // dsp thread

  /*one result  | 5 operands*/
  ISA_PRINT_TYPE c3type1 =  ISA_Print_Type_Create("c3type1", "%s %s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Instruction_Print_Group(c3type1,
  	        TOP_c3_mac_ar,  
               TOP_c3_macn_ar,
               TOP_c3_mula_ar,
               TOP_c3_samulh_a,
               TOP_c3_sasub_a,
               TOP_c3_sasubh_a,
               TOP_c3_saadd_a,
               TOP_c3_saaddh_a,
               TOP_UNDEFINED);

  /*one result  | 3 operands*/
  ISA_PRINT_TYPE c3type2 =  ISA_Print_Type_Create("c3type2", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(c3type2,
                     TOP_c3_bitc,  
                     TOP_c3_bitr,
  	             TOP_c3_dmula,
  	             TOP_c3_dmulan, 
  	             TOP_c3_mac,// same_res
  	             TOP_c3_macci, // same_res
  	             TOP_c3_maccr, //same_res
  	             TOP_c3_macd, //same_res
  	             TOP_c3_macn, // same_res
  	             TOP_c3_mula,
  	             TOP_c3_mulaci,
  	             TOP_c3_mulacr,
  	             TOP_c3_mulan, //same_res
  	             TOP_c3_dshll_i,
    	             TOP_c3_dshrl_i,
    	             TOP_c3_mac_i, //same_res
                    TOP_c3_macn_i, //same_res
                    TOP_c3_mula_i,
                    TOP_c3_saadds,
  	            TOP_c3_sasubs,
  	            TOP_c3_saaddsh,
  	            TOP_c3_sasubsh,
  	            TOP_c3_samulsh,
  	        TOP_UNDEFINED);

  /*one result  | 6 operands*/
  ISA_PRINT_TYPE c3type5=  ISA_Print_Type_Create("c3type5", "%s %s,%s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(5);
  Instruction_Print_Group(c3type5,
  	             TOP_c3_dmula_a,
  	             TOP_c3_dmulan_a,
  	             TOP_c3_mac_a,// same_res
  	             TOP_c3_macn_a, //same_res
  	             TOP_c3_mula_a,
  	             TOP_c3_saddha_a,
  	             TOP_UNDEFINED);

  /*one result  | 7 operands*/
  ISA_PRINT_TYPE c3type6=  ISA_Print_Type_Create("c3type6", "%s %s,%s,%s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(5);
  Operand(6);
  Instruction_Print_Group(c3type6,
                         TOP_c3_dmac_a,// same_res
                         TOP_c3_dmacn_a,// same_res
                         TOP_UNDEFINED);
 
  /*one result  | 4 operands*/
  ISA_PRINT_TYPE c3type7 =  ISA_Print_Type_Create("c3type7", "%s %s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(c3type7,
  	            TOP_c3_saddha,
                    TOP_c3_dmac,  // same_res
                    TOP_c3_dmacn,// same_res
  	            TOP_UNDEFINED);
  
  /*one result  | 2 operands*/
  ISA_PRINT_TYPE c3type8 =  ISA_Print_Type_Create("c3type8", "%s %s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group (c3type8,
  	            TOP_c3_lead,
  	            TOP_c3_revb,
  	            TOP_c3_sashllh,
                    TOP_c3_dadd,
                    TOP_c3_dsub,
 	            TOP_UNDEFINED);

  /*one result | two operand | no name()*/
  ISA_PRINT_TYPE c3type11 =  ISA_Print_Type_Create("c3type11", "c3.mvfs %s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(c3type11,
  	             TOP_c3_mvfacc,
  	             TOP_c3_mvfadd,
      	             TOP_c3_mvfadds,
  	             TOP_UNDEFINED);
  
  /*one result | two operand | no name()*/
  ISA_PRINT_TYPE c3type12 =  ISA_Print_Type_Create("c3type12", "c3.mvts %s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);  
  Instruction_Print_Group(c3type12,
     	             TOP_c3_mvtacc,
      	             TOP_c3_mvtadds,
      	             TOP_c3_mvtadd,
  	             TOP_UNDEFINED);

  ISA_PRINT_TYPE c3type13 = ISA_Print_Type_Create("c3type13", "%s %s, %s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(c3type13,
  	             TOP_c3_mvfs,
  	             TOP_c3_mvts,
  	             TOP_UNDEFINED);
  
  /*one result  | 2 operand*/
  ISA_PRINT_TYPE c3type14 =  ISA_Print_Type_Create("c3type14", "%s %s,%s,%s");
  Name();
  Result(0); // same res
  Operand(0); // shl
  Operand(1); // mode
  Instruction_Print_Group(c3type14,
  	             TOP_c3_round,
  	             TOP_UNDEFINED);

  /*no result | 5 operand*/
  ISA_PRINT_TYPE c3type22 =  ISA_Print_Type_Create("c3type22", "%s %s,%s,%s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(3);
  Operand(4);
  Operand(5);
  Instruction_Print_Group(c3type22,
  	             TOP_c3_st,
                     TOP_c3_fftst,
  	             TOP_UNDEFINED);
  
  ISA_PRINT_TYPE c3ld = ISA_Print_Type_Create("c3ld", "%s %s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Operand(3);
  Operand(4);
  Instruction_Print_Group(c3ld,
                     TOP_c3_ld,
                     TOP_c3_fftld,
                     TOP_UNDEFINED);

  // new c3 
  ISA_PRINT_TYPE c3_1des_4opr =  ISA_Print_Type_Create("c3_1des_4opr", "%s %s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(c3_1des_4opr,
                          TOP_C3_mac,
                          TOP_C3_macn,
                          TOP_C3_mula_ar,
                          TOP_C3_saadd_a,
                          TOP_C3_saaddh_a,
                          TOP_C3_samulh_a,
                          TOP_C3_sasub_a,
                          TOP_C3_sasubh_a,
                          TOP_C3_shlata_i,
                          TOP_C3_shrata_i,
                          TOP_C3_mac_i,
                          TOP_C3_macn_i,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_2des_2opr = ISA_Print_Type_Create("c3_2des_2opr", "%s %s,%s,%s");
  Name();
  Result(1);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(c3_2des_2opr,
                          TOP_C3_subc,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_2des_3opr = ISA_Print_Type_Create("c3_2des_3opr", "%s %s,%s,%s,%s");
  Name();
  Result(1);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(c3_2des_3opr,
                          TOP_C3_muls,
                          TOP_C3_mulus,
                          TOP_UNDEFINED);
   
  ISA_PRINT_TYPE c3_1des_3opr = ISA_Print_Type_Create("c3_1des_3opr", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(c3_1des_3opr,
                          TOP_C3_aadda,
                          TOP_C3_nega,
                          TOP_C3_bitr,
                          TOP_C3_cs,
                          TOP_C3_dshll_i,
                          TOP_C3_dshrl_i,
                          TOP_C3_mula,
                          TOP_C3_mulan,
                          TOP_C3_mula_i,
                          TOP_C3_round,
                          TOP_C3_saadds,
                          TOP_C3_saaddsh,
                          TOP_C3_sasubsh,
                          TOP_C3_samulsh,
                          TOP_C3_sasubs,
                          TOP_C3_shav,
                          TOP_C3_shlafa_i,
                          TOP_C3_shrafa_i,
                          TOP_C3_shla_i,
                          TOP_C3_shra_i,
                          TOP_UNDEFINED);
 
  ISA_PRINT_TYPE c3_1des_2opr = ISA_Print_Type_Create("c3_1des_2opr", "%s %s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(c3_1des_2opr,
                          TOP_C3_dadd,
                          TOP_C3_dsub,
                          TOP_C3_revb,
                          TOP_C3_mvfs,
                          TOP_C3_mvts,
                          TOP_UNDEFINED);


  ISA_PRINT_TYPE c3_gfs = ISA_Print_Type_Create("c3_gfs", "C3.mvfs %s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(c3_gfs,
                          TOP_C3_mvfacc,
                          TOP_C3_mvfaddr,
                          TOP_C3_mvfadds,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_gts = ISA_Print_Type_Create("c3_gts", "C3.mvts %s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(c3_gts,
                          TOP_C3_mvtacc,
                          TOP_C3_mvtaddr,
                          TOP_C3_mvtadds,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_1des_6opr = ISA_Print_Type_Create("c3_1des_6opr", "%s %s,%s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(5);
  Instruction_Print_Group(c3_1des_6opr,
                          TOP_C3_dmac,
                          TOP_C3_dmacn,
                          TOP_C3_dmula_a,
                          TOP_C3_dmulan_a,
                          TOP_C3_mac_a,
                          TOP_C3_macn_a,
                          TOP_C3_sadda,
	                  TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_1des_7opr = ISA_Print_Type_Create("c3_1des_7opr", "%s %s,%s,%s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(5);
  Operand(6);
  Instruction_Print_Group(c3_1des_7opr,
                          TOP_C3_dmac_a,
                          TOP_C3_dmacn_a,
                          TOP_C3_sadda_a,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_1des_5opr = ISA_Print_Type_Create("c3_1des_5opr", "%s %s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Instruction_Print_Group(c3_1des_5opr,
                          TOP_C3_dmula,
                          TOP_C3_dmulan,
                          TOP_C3_mac_ar,
                          TOP_C3_macn_ar,
                          TOP_C3_mula_a,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_0des_3opr =  ISA_Print_Type_Create("c3_0des_3opr", "%s %s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(c3_0des_3opr,
                          TOP_C3_ffe,
                          TOP_UNDEFINED);
   
  ISA_PRINT_TYPE c3_1des_3opr_1 = ISA_Print_Type_Create("c3_1des_3opr_1", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(c3_1des_3opr_1,
                     TOP_C3_ld,
                     TOP_C3_fftld,
                     TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_0des_4opr =  ISA_Print_Type_Create("c3_0des_4opr", "%s %s,%s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(3);
  Operand(4);
  Instruction_Print_Group(c3_0des_4opr,
                     TOP_C3_st,
                     TOP_C3_fftst,
                     TOP_UNDEFINED);

  ISA_PRINT_TYPE c3_1des_1opr = ISA_Print_Type_Create("c3_1des_1opr", "%s %s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(c3_1des_1opr,
                     TOP_C3_lead,
                     TOP_UNDEFINED);
  // end
  
  // core thread
  ISA_PRINT_TYPE slad =  ISA_Print_Type_Create("slad", "add %s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(slad,
                          TOP_addu,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE zdloop = ISA_Print_Type_Create("zdloop", "%s %s, %s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(zdloop, 
  	                  TOP_loop,
  	                  TOP_UNDEFINED);
  
  ISA_PRINT_TYPE mvcg =  ISA_Print_Type_Create("mvcg", "%s %s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(mvcg,
                          TOP_mvtc,
                          TOP_mvfc,
                          TOP_mvtc_i,
                          TOP_mvtc16,
                          TOP_mvfc16,
                          TOP_mv16,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE c3muls =  ISA_Print_Type_Create("c3muls", "%s %s,%s,%s,%s");
  Name();
  Result(1);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(mvcg,
                          TOP_c3_muls,
                          TOP_c3_mulus,
                          TOP_UNDEFINED);

  
  /* 16-bit instruction*/
  /* one result  / one operand  */
  ISA_PRINT_TYPE sixteenop0 = ISA_Print_Type_Create("sixteenop0", "%s %s,%s");
  Name();
  Result(0);
  Operand(1);
  Instruction_Print_Group(sixteenop0,
                          TOP_add16,
	                  TOP_add16_i,
	                  TOP_and16,
	                  TOP_and16_i,
	                  TOP_or16,
	                  TOP_or16_i,
	                  TOP_shll16,
	                  TOP_shll16_i,
	                  TOP_shra16,
                          TOP_shra16_i,
	                  TOP_shrl16,
	                  TOP_shrl16_i,
	                  TOP_sub16,
	                  TOP_sub16_i,
	                  TOP_xor16,
	                  TOP_xor16_i,
                          TOP_mv16_i,
	                  TOP_UNDEFINED);

  //add temp
  ISA_PRINT_TYPE sixteenop3 =  ISA_Print_Type_Create("sixteenop3", "%s %s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(sixteenop3,
  	                  TOP_ldw16,
  	                  TOP_UNDEFINED);

  ISA_PRINT_TYPE sixteenop4 =  ISA_Print_Type_Create("sixteenop4", "%s %s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(sixteenop4,
  	                  TOP_stw16,
                          TOP_c3_trback,
                          TOP_c3_viterbi,
                          TOP_c3_fft,
  	                  TOP_UNDEFINED);

  ISA_PRINT_TYPE sixteenop5 =  ISA_Print_Type_Create("sixteenop5", "%s %s,%s");
  Name();
  Operand(0);
  Operand(2);
  Instruction_Print_Group(sixteenop5,
                           TOP_br16_eqz,
                          TOP_br16_nez,
  	                  TOP_UNDEFINED);

  /* one operand / no result */
  ISA_PRINT_TYPE sixteenop1 = ISA_Print_Type_Create("sixeenop1", "%s %s");
  Name();
  Operand(1);
  Instruction_Print_Group(sixteenop1,
                          TOP_add16_sp,
                          TOP_UNDEFINED);

  ISA_PRINT_TYPE sixteenop101 = ISA_Print_Type_Create("sixeenop101", "%s %s");
  Name();
  Operand(0);
  Instruction_Print_Group(sixteenop101,
  	                  TOP_abs16,
  	                  TOP_ldub16_rs,
                          TOP_lduh16_rs,
                          TOP_inv16,
#ifdef TARG_SL2 
                          TOP_c2_fork_m,
			  TOP_c2_fork_n,
			  TOP_c2_thctrl_act,
			  TOP_c2_thctrl_mode4, 
			  TOP_c2_thctrl_mode5,
			  TOP_c2_thctrl_mode6,
			  TOP_c2_thctrl_mode7,
#endif                           
			  TOP_UNDEFINED);

  /* no operand / no result*/
  ISA_PRINT_TYPE sixteenop2 = ISA_Print_Type_Create("sixteenop2", "%s" );
  Name();
  Instruction_Print_Group(sixteenop2,
		  TOP_jr16,
		  TOP_jr16_lnk,
		  TOP_nop16,
		  TOP_ret16,
		  TOP_UNDEFINED);

  /* Two operand / no result  */
  ISA_PRINT_TYPE push16 =  ISA_Print_Type_Create("push16", "%s %s,%s");
  Name();                  
  Operand(0);              
  Operand(2);
  Instruction_Print_Group(push16,
		  TOP_push16,
                          TOP_UNDEFINED);
             
  /* One operand / one result */
  ISA_PRINT_TYPE poppush =  ISA_Print_Type_Create("pop16", "%s %s,%s");
  Name();    
  Result(0); 
  Operand(1);
  Instruction_Print_Group(poppush,
                          TOP_pop16,
                          TOP_UNDEFINED);
  
#endif 
  /* One result / two operands */
#if defined(TARG_SL)
  ISA_PRINT_TYPE rsub =  ISA_Print_Type_Create("rsub", "sub %s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(rsub,
                          TOP_subu,
                          TOP_UNDEFINED);
#endif

#if defined(TARG_SL2)
  ISA_PRINT_TYPE ropop2 =  ISA_Print_Type_Create("ropop", "%s %s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Instruction_Print_Group(ropop2,
                          TOP_c2_mvgr_r2g_w,
			  TOP_c2_mvgr_r2g_w_i,
			  TOP_UNDEFINED);
#endif  

  ISA_PRINT_TYPE ropop =  ISA_Print_Type_Create("ropop", "%s %s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(ropop,
	  		  TOP_add,
#if !defined(TARG_SL)
			  TOP_addu,
                          TOP_subu,
#endif
			  TOP_sub,
			  TOP_dadd,
			  TOP_daddu,
			  TOP_dsub,
			  TOP_dsubu,
			  TOP_and,
			  TOP_nor,
			  TOP_or,
			  TOP_xor,
			  TOP_sllv,
			  TOP_srav,
			  TOP_srlv,
			  TOP_dsllv,
			  TOP_dsrav,
			  TOP_dsrlv,
			  TOP_slt,
			  TOP_sltu,
			  TOP_add_s,
			  TOP_add_d,
			  TOP_div_s,
			  TOP_div_d,
			  TOP_mul_s,
			  TOP_mul_d,
			  TOP_sub_s,
			  TOP_sub_d,
			  TOP_addi,
			  TOP_addiu,
			  TOP_slti,
			  TOP_sltiu,
			  TOP_daddi,
			  TOP_daddiu,
			  TOP_andi,
			  TOP_ori,
			  TOP_xori,
			  TOP_sll,
			  TOP_sra,
			  TOP_srl,
			  TOP_dsll,
			  TOP_dsll32,
			  TOP_dsra,
			  TOP_dsra32,
			  TOP_dsrl,
			  TOP_dsrl32,
			  TOP_movf,
			  TOP_movn,
			  TOP_movt,
			  TOP_movz,
			  TOP_c_f_s,
			  TOP_c_f_d,
			  TOP_c_t_s,
			  TOP_c_t_d,
			  TOP_c_un_s,
			  TOP_c_un_d,
			  TOP_c_or_s,
			  TOP_c_or_d,
			  TOP_c_eq_s,
			  TOP_c_eq_d,
			  TOP_c_neq_s,
			  TOP_c_neq_d,
			  TOP_c_ueq_s,
			  TOP_c_ueq_d,
			  TOP_c_olg_s,
			  TOP_c_olg_d,
			  TOP_c_olt_s,
			  TOP_c_olt_d,
			  TOP_c_uge_s,
			  TOP_c_uge_d,
			  TOP_c_ult_s,
			  TOP_c_ult_d,
			  TOP_c_oge_s,
			  TOP_c_oge_d,
			  TOP_c_ole_s,
			  TOP_c_ole_d,
			  TOP_c_ugt_s,
			  TOP_c_ugt_d,
			  TOP_c_ule_s,
			  TOP_c_ule_d,
			  TOP_c_ogt_s,
			  TOP_c_ogt_d,
			  TOP_c_sf_s,
			  TOP_c_sf_d,
			  TOP_c_st_s,
			  TOP_c_st_d,
			  TOP_c_ngle_s,
			  TOP_c_ngle_d,
			  TOP_c_gle_s,
			  TOP_c_gle_d,
			  TOP_c_seq_s,
			  TOP_c_seq_d,
			  TOP_c_sne_s,
			  TOP_c_sne_d,
			  TOP_c_ngl_s,
			  TOP_c_ngl_d,
			  TOP_c_gl_s,
			  TOP_c_gl_d,
			  TOP_c_lt_s,
			  TOP_c_lt_d,
			  TOP_c_nlt_s,
			  TOP_c_nlt_d,
			  TOP_c_nge_s,
			  TOP_c_nge_d,
			  TOP_c_ge_s,
			  TOP_c_ge_d,
			  TOP_c_le_s,
			  TOP_c_le_d,
			  TOP_c_nle_s,
			  TOP_c_nle_d,
			  TOP_c_ngt_s,
			  TOP_c_ngt_d,
			  TOP_c_gt_s,
			  TOP_c_gt_d,
			  TOP_movf_s,
			  TOP_movf_d,
			  TOP_movn_s,
			  TOP_movn_d,
			  TOP_movt_s,
			  TOP_movt_d,
			  TOP_movz_s,
			  TOP_movz_d,
			  TOP_UNDEFINED);

  /* no result / no operand*/
#ifdef TARG_SL
  ISA_PRINT_TYPE sljr =  ISA_Print_Type_Create("op", "%s ");
  Name();
  Instruction_Print_Group(sljr,
		  TOP_jr,
		  TOP_jalr,
		  TOP_UNDEFINED);
#endif

  /* No result / one operand */
  ISA_PRINT_TYPE op =  ISA_Print_Type_Create("op", "%s %s");
  Name();
  Operand(0);
  Instruction_Print_Group(op,
		  TOP_j,
		  TOP_jal,
#if !defined(TARG_SL)
		  TOP_jalr,
		  TOP_jr,
#endif
		  TOP_mthi,
		  TOP_mtlo,
		  TOP_UNDEFINED);

  /* No result / two operands */
  ISA_PRINT_TYPE opop =  ISA_Print_Type_Create("opop", "%s %s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(opop,
		  TOP_mult,
		  TOP_multu,
		  TOP_dmult,
		  TOP_dmultu,
		  TOP_bgez,
		  TOP_bgezal,
		  TOP_bgtz,
		  TOP_blez,
		  TOP_bltz,
		  TOP_bltzal,
		  TOP_bc1f,
		  TOP_bc1t,
		  TOP_UNDEFINED);




  /* No result / two operands extra $0 operand for divide */
  ISA_PRINT_TYPE divide =  ISA_Print_Type_Create("divide", "%s $0,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(divide,
		  TOP_div,
		  TOP_divu,
		  TOP_ddiv,
		  TOP_ddivu,
		  TOP_UNDEFINED);

  /* One result / no operand */
  ISA_PRINT_TYPE r =  ISA_Print_Type_Create("r", "%s %s");
  Name();
  Result(0);
  Instruction_Print_Group(r,
		  TOP_mfhi,
		  TOP_mflo,
#ifdef TARG_SL2
		  TOP_c2_vlcs_wb,
#endif
		  TOP_UNDEFINED);

  /* One result / one operand */
  ISA_PRINT_TYPE rop =  ISA_Print_Type_Create("rop", "%s %s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(rop,
			  TOP_lui,
			  TOP_neg_s,
			  TOP_neg_d,
			  TOP_sqrt_s,
			  TOP_sqrt_d,
			  TOP_abs_s,
			  TOP_abs_d,
			  TOP_recip_s,
			  TOP_recip_d,
			  TOP_rsqrt_s,
			  TOP_rsqrt_d,
			  TOP_mov_s,
			  TOP_mov_d,
			  TOP_mfc1,
			  TOP_dmfc1,
			  TOP_cvt_d_s,
			  TOP_cvt_d_w,
			  TOP_cvt_d_l,
			  TOP_cvt_l_s,
			  TOP_cvt_l_d,
			  TOP_cvt_s_d,
			  TOP_cvt_s_w,
			  TOP_cvt_s_l,
			  TOP_cvt_w_s,
			  TOP_cvt_w_d,
			  TOP_ceil_w_s,
			  TOP_ceil_w_d,
			  TOP_ceil_l_s,
			  TOP_ceil_l_d,
			  TOP_floor_w_s,
			  TOP_floor_w_d,
			  TOP_floor_l_s,
			  TOP_floor_l_d,
			  TOP_round_w_s,
			  TOP_round_w_d,
			  TOP_round_l_s,
			  TOP_round_l_d,
			  TOP_trunc_w_s,
			  TOP_trunc_w_d,
			  TOP_trunc_l_s,
			  TOP_trunc_l_d,
#ifdef TARG_SL2
                          TOP_c2_vclp,
			  TOP_c2_vclp_p,
			  TOP_c2_vabs_p,
			  TOP_c2_vabs_p_sm,
			  TOP_c2_vabs_h,
			  TOP_c2_vabs_h_sm,
			  TOP_c2_vabs_w,
			  TOP_c2_vabs_w_sm,
			  TOP_c2_vcopy, 
			  TOP_c2_mvgr_g2r_ba_lh,
			  TOP_c2_mvgr_g2r_ba_hh,
			  TOP_c2_mvgr_g2r_ba_w,
			  TOP_c2_mvgr_g2r_bh,
			  TOP_c2_mvgr_g2r_bh_u,
			  TOP_c2_mvgr_g2r_bv,
			  TOP_c2_mvgr_g2r_bv_u,
			  TOP_c2_mvgc_c2g,
			  TOP_c2_mvgc_g2c,
			  TOP_c2_ldi_s_h_u,
			  TOP_c2_ldi_s_h,
			  TOP_c2_ldi_s_w,
			  TOP_c2_ldi_c,
			  TOP_c2_ldi_v_b_u,
			  TOP_c2_ldi_v_b,
			  TOP_c2_ldi_v_h,
			  TOP_c2_ldi_v_w,
			  TOP_c2_ldi_v_m_b_u,
			  TOP_c2_ldi_v_m_b,
			  TOP_c2_ldi_v_m_h,
			  TOP_c2_ldi_v_m_w,
			  TOP_c2_bcst_q,
			  TOP_c2_bcst_i,
			  TOP_c2_vlcs_dc,
			  TOP_c2_vlcs_ac,
			  TOP_c2_mov_g,
			  TOP_c2_mov_r ,
			  TOP_c2_ldi_v2g_b_u, 
			  TOP_c2_ldi_v2g_b,   
			  TOP_c2_ldi_v2g_h_u, 
			  TOP_c2_ldi_v2g_h,   
			  TOP_c2_ldi_v2g_w,   
			  TOP_c2_lczero_z,
			  TOP_c2_lczero_nz_fw, 
			  TOP_c2_lczero_nz_bw,

#endif //TARG_SL2
			  TOP_UNDEFINED);

#if defined(TARG_SL)
  /* c2.load */
  ISA_PRINT_TYPE c2ldv2g =  ISA_Print_Type_Create("c2ldv2g", "%s %s,%s(%s)");
  Name();
  Result(0);
  Operand(1);
  Operand(0);
  Instruction_Print_Group(c2ldv2g,
			  TOP_c2_ld_v2g_b_u,
			  TOP_c2_ld_v2g_b,
			  TOP_c2_ld_v2g_h_u, 
			  TOP_c2_ld_v2g_h,   
			  TOP_c2_ld_v2g_w,   
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE c2stg2v =  ISA_Print_Type_Create("c2stg2v", "%s %s,%s(%s)");
  Name();
  Operand(0);
  Operand(2);
  Operand(1);
  Instruction_Print_Group(c2stg2v,
			  TOP_c2_st_g2v_b, 
			  TOP_c2_st_g2v_h, 
			  TOP_c2_st_g2v_w, 
			  TOP_UNDEFINED);
#endif //TARG_SL 


  /* One operand / one result */
  ISA_PRINT_TYPE opr =  ISA_Print_Type_Create("opr", "%s %s,%s");
  Name();
  Operand(0);
  Result(0);
  Instruction_Print_Group(opr,
		  TOP_mtc1,
		  TOP_dmtc1,
		  TOP_UNDEFINED);
#if defined(TARG_SL)
  /* One operand / one result */
  ISA_PRINT_TYPE oprfcc =  ISA_Print_Type_Create("oprfcc", "%s %s,$31");
  Name();
  Result(0);
  Instruction_Print_Group(oprfcc,
		  TOP_cfc1,
		  TOP_UNDEFINED);
  /* One operand / one result */
  ISA_PRINT_TYPE opr2fcc =  ISA_Print_Type_Create("opr2fcc", "%s %s,$31");
  Name();
  Operand(0);
  Instruction_Print_Group(opr2fcc,
		  TOP_ctc1,
		  TOP_UNDEFINED);
#endif

  /* One result / three operands */
  ISA_PRINT_TYPE ropopop =  ISA_Print_Type_Create("ropopop", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(ropopop,
		  TOP_madd_s,
		  TOP_madd_d,
		  TOP_msub_s,
		  TOP_msub_d,
		  TOP_nmadd_s,
		  TOP_nmadd_d,
		  TOP_nmsub_s,
		  TOP_nmsub_d,
#ifdef TARG_SL2
		  TOP_c2_vspel_adds,
		  TOP_c2_vspel_mac_h,
		  TOP_c2_smads,
		  TOP_c2_cmov,
		  TOP_c2_clp,
		  TOP_c2_clp_i,
		  TOP_c2_chkrng,
		  TOP_c2_bxtr_u_l,
		  TOP_c2_bxtr_s_l,
		  TOP_c2_bxtr_u_m,
		  TOP_c2_bxtr_s_m,
		  TOP_c2_bxtrr48,
		  TOP_c2_bxtrr48_i,
		  TOP_c2_sum4_r,
		  TOP_c2_med,
		  TOP_c2_subs_g_i,
		  TOP_c2_subs_g,
		  TOP_c2_subs_r_h_i,
		  TOP_c2_subs_r_h,
		  TOP_c2_vsubs_h_abs,
		  TOP_c2_vsubs_h_abs_sm,
		  TOP_c2_mov_s_i,
		  TOP_c2_mov_s,
		  TOP_c2_sum4_saddr, 
		  TOP_c2_vmov_swin,
                  TOP_c2_bdep_l,
                  TOP_c2_bdep_m,
                  TOP_c2_shor_l,
                  TOP_c2_shor_rl,
                  TOP_c2_shor_ra,
                  TOP_c2_shadd_l,
                  TOP_c2_shadd_rl,
                  TOP_c2_shadd_ra,
                  TOP_c2_shsub_l,
                  TOP_c2_shsub_rl, 
                  TOP_c2_shsub_ra, 
                  TOP_c2_shor_l_i,
                  TOP_c2_shor_rl_i,
                  TOP_c2_shor_ra_i,
                  TOP_c2_shadd_l_i,
                  TOP_c2_shadd_rl_i,
                  TOP_c2_shadd_ra_i,
                  TOP_c2_shsub_l_i,
                  TOP_c2_shsub_rl_i, 
                  TOP_c2_shsub_ra_i, 
#endif //TARG_SL2
		  TOP_UNDEFINED);


#ifdef TARG_SL2
  ISA_PRINT_TYPE rropop =  ISA_Print_Type_Create("rropop", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Result(1);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(rropop,
                          TOP_c2_sad, 
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE ropoprop =  ISA_Print_Type_Create("ropoprop", "%s %s,%s,%s,%s,%s");
  Name();
  Result(0);
  Result(1);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(ropoprop,
                          TOP_c2_satd, 
			  TOP_UNDEFINED);


  ISA_PRINT_TYPE mvsel =  ISA_Print_Type_Create("mvsel", "c2.mvsel %s,%s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(mvsel,
                          TOP_c2_mvsel_mode0,   
			  TOP_c2_mvsel_mode1, 
			  TOP_c2_mvsel_mode2,   
			  TOP_c2_mvsel_mode345, 
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE rop0op2op3 =  ISA_Print_Type_Create("rop0op2op3", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(rop0op2op3,
                          TOP_c2_subs_r_w_i,
			  TOP_c2_vspas,
			  TOP_c2_vspel_mac_w,
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE rop0op2op4 =  ISA_Print_Type_Create("rop0op2op3", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Operand(4);
  Instruction_Print_Group(rop0op2op3,
                          TOP_c2_subs_r_w,
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE ropopopop =  ISA_Print_Type_Create("ropopopop", "%s %s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(ropopopop,
                          TOP_c2_vadds_h,
			  TOP_c2_vadds_h_mode6,
			  TOP_c2_vadds_h_mode2,
			  TOP_c2_vsubs_h,
			  TOP_c2_vsubs_h_sm,
			  TOP_c2_mmul_h,
			  TOP_c2_gsums,
			  TOP_c2_vsubs_h_abs,
			  TOP_c2_vsubs_h_abs_sm,
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE vadds_h =  ISA_Print_Type_Create("vadds_h", "c2.vadds.h %s,%s,%s,%s,%s");
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(vadds_h,
                         TOP_c2_vadds_h,
			 TOP_c2_vadds_h_mode6,
			 TOP_c2_vadds_h_mode2,
			 TOP_UNDEFINED);

  ISA_PRINT_TYPE vadds_w =  ISA_Print_Type_Create("vadds_w", "c2.vadds.w %s,%s,%s,%s,%s");
  Result(0);
  Operand(0);
  Operand(2);
  Operand(4);
  Operand(5);
  Instruction_Print_Group(vadds_w,
		        TOP_c2_vadds_w,
			TOP_c2_vadds_w_mode6,
			TOP_c2_vadds_w_mode2,
			TOP_UNDEFINED);

  ISA_PRINT_TYPE vadds_p =  ISA_Print_Type_Create("vadds_p", "c2.vadds.p %s,%s,%s,%s,%s");
  Result(0);
  Operand(0);
  Operand(2);
  Operand(4);
  Operand(5);
  Instruction_Print_Group(vadds_p,
                       TOP_c2_vadds_p,
		       TOP_c2_vadds_p_mode6,
		       TOP_c2_vadds_p_mode2,
		       TOP_UNDEFINED);

  ISA_PRINT_TYPE rop0op2op4op5 =  ISA_Print_Type_Create("rop0op2op4op5", "%s %s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Operand(4);
  Operand(5);
  Instruction_Print_Group(rop0op2op4op5,
                       TOP_c2_vsubs_w,
		       TOP_c2_vsubs_w_sm,
		       TOP_c2_vsubs_p,
		       TOP_c2_vsubs_p_sm,
		       TOP_c2_vsubs_w_abs,	        
		       TOP_c2_vsubs_w_abs_sm,	        
		       TOP_c2_vsubs_p_abs,
		       TOP_c2_vsubs_p_abs_sm,
		       TOP_c2_mmul_w,
		       TOP_UNDEFINED);

  ISA_PRINT_TYPE ropopopopop =  ISA_Print_Type_Create("ropopopop", "%s %s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Instruction_Print_Group(ropopopopop,
                          TOP_c2_mads,
			  TOP_c2_add_shl_g_i,
			  TOP_c2_add_shr_g_i,
			  TOP_c2_add_shl_g,
			  TOP_c2_add_shr_g,
			  TOP_c2_add_shl_r_h_i,
			  TOP_c2_add_shr_r_h_i,
			  TOP_c2_add_shl_r_h,
			  TOP_c2_add_shr_r_h,
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE rop0op2op3op4op5 =  ISA_Print_Type_Create("rop0op2op3op4op5", "%s %s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Operand(3);
  Operand(4);
  Operand(5);
  Instruction_Print_Group(ropopopopop,
		          TOP_c2_add_shl_r_w_i,
			  TOP_c2_add_shr_r_w_i,
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE rop0op2op4op5op6 =  ISA_Print_Type_Create("rop0op2op4op5op6", "%s %s,%s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(2);
  Operand(4);
  Operand(5);
  Operand(6);
  Instruction_Print_Group(ropopopopop,
                          TOP_c2_add_shl_r_w,
			  TOP_c2_add_shr_r_w,
			  TOP_UNDEFINED);


  ISA_PRINT_TYPE c2muls =  ISA_Print_Type_Create("c2muls", "c2.mul.s %s,%s,%s,%s,%s,%s");
  //Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Instruction_Print_Group(c2muls,
                          TOP_c2_muls,
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE load_no_off =  ISA_Print_Type_Create("load_no_off", "%s %s,(%s)");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(load_no_off,
                          TOP_c2_ld_v_b_u,
			  TOP_c2_ld_v_b,
			  TOP_c2_ld_v_h,
			  TOP_c2_ld_v_w,
			  TOP_c2_ld_v_sw,
			  TOP_c2_ld_v_m_b_u,
			  TOP_c2_ld_v_m_b,
			  TOP_c2_ld_v_m_h,
			  TOP_c2_ld_v_m_w,
			  TOP_c2_ld_s_h_u_p,
			  TOP_c2_ld_s_h_u,
			  TOP_c2_ld_s_h_p,
			  TOP_c2_ld_s_h,
			  TOP_c2_ld_s_w_p,
			  TOP_c2_ld_s_w,
			  TOP_UNDEFINED);

#endif //TARG_SL2

  /* No result / three operands */
  ISA_PRINT_TYPE opopop =  ISA_Print_Type_Create("opopop", "%s %s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(opopop,
		  TOP_beq,
			  TOP_bne,
			  TOP_teq,
			  TOP_teqi,
			  TOP_tge,
			  TOP_tgei,
			  TOP_tgeiu,
			  TOP_tgeu,
			  TOP_tlt,
			  TOP_tlti,
			  TOP_tltiu,
			  TOP_tltu,
			  TOP_tne,
			  TOP_tnei,
			  TOP_UNDEFINED);


  
  /* regular load */
  ISA_PRINT_TYPE load =  ISA_Print_Type_Create("load", "%s %s,%s(%s)");
  Name();
  Result(0);
  Operand(1);
  Operand(0);
  Instruction_Print_Group(load,
			  TOP_lb,
			  TOP_lbu,
			  TOP_lh,
			  TOP_lhu,
			  TOP_lw,
			  TOP_lwl,
			  TOP_lwr,
			  TOP_ll,
			  TOP_lwu,
			  TOP_ld,
			  TOP_ldl,
			  TOP_ldr,
			  TOP_lld,
			  TOP_lwc1,
			  TOP_ldc1,
			  TOP_UNDEFINED);

  /* prefetch */
  ISA_PRINT_TYPE prefetch =  ISA_Print_Type_Create("prefetch", "%s %s,%s(%s)");
  Name();
  Operand(0);
  Operand(2);
  Operand(1);
  Instruction_Print_Group(prefetch,
			  TOP_pref,
			  TOP_UNDEFINED);

#ifdef TARG_SL
  ISA_PRINT_TYPE store_no_off =  ISA_Print_Type_Create("store_no_off", "%s %s,(%s)");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(store_no_off,
#ifdef TARG_SL2 
                         TOP_c2_st_v_b,
			 TOP_c2_st_v_h,
			 TOP_c2_st_v_w,
			 TOP_c2_st_v_m_b,
			 TOP_c2_st_v_m_h,
			 TOP_c2_st_v_m_w,
			 TOP_c2_st_s_h,
			 TOP_c2_st_s_h_p,
			 TOP_c2_st_s_w,
			 TOP_c2_st_s_w_p,

#endif //TARG_SL2
			 TOP_UNDEFINED);
#endif

  ISA_PRINT_TYPE store_pair_no_off =  ISA_Print_Type_Create("store_no_off", "%s %s,(%s)");
  Name();
  Operand(0);
  Operand(2);
  Instruction_Print_Group(store_pair_no_off,
#ifdef TARG_SL2 
		          TOP_c2_st_v_w,
			  TOP_c2_st_v_m_w,
#endif //TARG_SL2
			  TOP_UNDEFINED);


  /* regular store */
  ISA_PRINT_TYPE store =  ISA_Print_Type_Create("store", "%s %s,%s(%s)");
  Name();
  Operand(0);
  Operand(2);
  Operand(1);
  Instruction_Print_Group(store,
			  TOP_sb,
			  TOP_sh,
			  TOP_sw,
			  TOP_swl,
			  TOP_swr,
			  TOP_sc,
			  TOP_sd,
			  TOP_sdl,
			  TOP_sdr,
			  TOP_scd,
			  TOP_swc1,
			  TOP_sdc1,
			  TOP_UNDEFINED);

  /* No results / no operands TODO */
  ISA_PRINT_TYPE no_rop = ISA_Print_Type_Create("no_rop", "%s");
  Name();
  Instruction_Print_Group(no_rop,
	  		  TOP_nop,
			  TOP_sync,
			  TOP_break,
			  TOP_syscall,

			  TOP_lwxc1,
			  TOP_ldxc1,
			  TOP_swxc1,
			  TOP_sdxc1,
#if !defined(TARG_SL)
			  TOP_cfc1,
			  TOP_ctc1,
#endif

#ifdef TARG_SL
                       	  TOP_ret,
#endif
			  TOP_prefx,
#ifdef TARG_SL2
                         TOP_c2_thctrl_lock,
			 TOP_c2_thctrl_unlock,
			 TOP_c2_thctrl_deact,
			 TOP_c2_joint,
			 TOP_c2_macro, 
#endif //TARG_SL2
			  TOP_UNDEFINED);

#if defined(TARG_SL)
  /* mc.abs */
  ISA_PRINT_TYPE mc_abs = ISA_Print_Type_Create("mc_abs","%s %s, %s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(mc_abs,
   	                  TOP_mc_abs,
   	                  TOP_UNDEFINED);
  /* mc.z, mc.zn mc.r*/
  ISA_PRINT_TYPE slmc = ISA_Print_Type_Create("slmc", "%s %s,%s,%s, %s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);

  Instruction_Print_Group(slmc,
		         TOP_mc_z_eq,
			  TOP_mc_z_ne,
			  TOP_mc_z_gt,
			  TOP_mc_z_ge,
			  TOP_mc_z_lt,
			  TOP_mc_z_le,
			  TOP_mc_zn_eq,
			  TOP_mc_zn_ne,
			  TOP_mc_zn_gt,
			  TOP_mc_zn_ge,
			  TOP_mc_zn_lt,
			  TOP_mc_zn_le,
			  TOP_mc_zc_eq,
			  TOP_mc_zc_ne,
			  TOP_mc_zc_gt,
			  TOP_mc_zc_ge,
			  TOP_mc_zc_lt,
			  TOP_mc_zc_le,
            		  TOP_mc_r_eq,
			  TOP_mc_r_ne,
			  TOP_mc_r_gt,
			  TOP_mc_r_ge,
			  TOP_mc_r_lt,
			  TOP_mc_r_le,
			  TOP_UNDEFINED);


  /* Extract bit */
  ISA_PRINT_TYPE extract_bit = ISA_Print_Type_Create("extract_bit", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0); 
  Operand(1);
  Operand(2);
  Instruction_Print_Group(extract_bit,
	                  TOP_extrbs,
			  TOP_extrbu,
			  TOP_UNDEFINED);	
			  
  /* Depost bit */
  ISA_PRINT_TYPE deposit_bit = ISA_Print_Type_Create("deposit_bit", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(1); //0 is hidden operand, = result
  Operand(2);
  Operand(3);
  Instruction_Print_Group(deposit_bit,
			  TOP_depb,
			  TOP_UNDEFINED);			  

#endif
  ISA_Print_End();
}
