/*
 * Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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
// Generate an ISA containing and group TOPS with similar operands/results 
// format.
/////////////////////////////////////////////////////////
// The instructions are listed below in the order as shown below. 
//
//   1. Real and pseudo instructions
//   2. Simulated instructions.
//
// Within each category, the instructions are arranged roughly in order
// of increasing numbers of operands.
/////////////////////////////////////
//
//  $Revision: 1.16 $
//  $Date: 2006/05/30 06:52:26 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa_operands.cxx,v $

#include "topcode.h"
#include "isa_operands_gen.h"

main()
{
  ISA_Operands_Begin("PPC32");

  /* Literals... */

  OPERAND_VALUE_TYPE simm16, uimm16, uimm1, uimm2, uimm3, uimm5, uimm7, uimm9,  \
  uimm10, uimm11, uimm14, uid2, uid3, uia1, uia3, uib1, uib3, uic1, uic3, \
  pc14, pcrel16, pc26;

  simm16 = ISA_Lit_Opnd_Type_Create("simm16", 16, SIGNED, LC_simm16);
  uimm16 = ISA_Lit_Opnd_Type_Create("uimm16", 16, UNSIGNED, LC_uimm16);
  uimm5  = ISA_Lit_Opnd_Type_Create("uimm5", 5, UNSIGNED, LC_uimm5);
  pc14   = ISA_Lit_Opnd_Type_Create("pc14", 14, PCREL, LC_pc14);
  pcrel16 = ISA_Lit_Opnd_Type_Create("pcrel16", 16, PCREL, LC_pcrel16);
  pc26 = ISA_Lit_Opnd_Type_Create("pc26", 26, PCREL, LC_pc26);
  uimm10 = ISA_Lit_Opnd_Type_Create("uimm10", 10, UNSIGNED, LC_uimm10);
  uimm3  = ISA_Lit_Opnd_Type_Create("uimm3", 3, UNSIGNED, LC_uimm3);
  uimm7  = ISA_Lit_Opnd_Type_Create("uimm7", 7, UNSIGNED, LC_uimm3);
  
  /* Registers... */

  OPERAND_VALUE_TYPE int32, fp64, cr, sr_ctr, sr_lr;

  int32 = ISA_Reg_Opnd_Type_Create("int32", ISA_REGISTER_CLASS_integer,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
  fp64 = ISA_Reg_Opnd_Type_Create("fp64", ISA_REGISTER_CLASS_float,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				64, SIGNED, INVALID);
  cr  = ISA_Reg_Opnd_Type_Create("cr", ISA_REGISTER_CLASS_condition,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				4, UNSIGNED, INVALID);
  sr_ctr = ISA_Reg_Opnd_Type_Create("sr_ctr", ISA_REGISTER_CLASS_special,
				ISA_REGISTER_SUBCLASS_sr_ctr,
				32, UNSIGNED, INVALID);
  sr_lr = ISA_Reg_Opnd_Type_Create("sr_lr", ISA_REGISTER_CLASS_special,
				ISA_REGISTER_SUBCLASS_sr_lr,
				32, UNSIGNED, INVALID);				

  /* Operand uses... */
  OPERAND_USE_TYPE
	  predicate,	// a qualifying predicate
	  base,		// a base address (for memory insts)
	  offset,	// an offset added to a base (implies immed val)
	  target,	// the target of a branch
	  storeval,	// value to be stored
	  opnd1,	// first/left operand of an alu operator
	  opnd2,	// second/right operand of an alu operator
	  opnd3,	// 3rd operand of an alu operator
    opnd4,
    opnd5,
	  maddend,	// addend/subtrahend operand of a madd
	  pos,			//ExDe Bit position
	  width;		//ExDe Bit width

  predicate  = Create_Operand_Use("predicate");
  base       = Create_Operand_Use("base");
  offset     = Create_Operand_Use("offset");
  target     = Create_Operand_Use("target");
  storeval   = Create_Operand_Use("storeval");
  opnd1      = Create_Operand_Use("opnd1");
  opnd2      = Create_Operand_Use("opnd2");
  opnd3      = Create_Operand_Use("opnd3");
  opnd4      = Create_Operand_Use("opnd4");

  maddend    = Create_Operand_Use("maddend");
  pos    		= Create_Operand_Use("pos");
  width    	= Create_Operand_Use("width");

  Instruction_Group("no operand",
		    TOP_nop,
		    TOP_noop,
		    TOP_sc,
     	  TOP_asm,
     	  TOP_blr,
     	  TOP_blrl,
     	  TOP_bctr,
     	  TOP_bctrl,
		    TOP_bwd_bar,
		    TOP_fwd_bar,
		    TOP_UNDEFINED);

/*
  Instruction_Group("b lr",
        TOP_blr,
		    TOP_blrl,
		    TOP_UNDEFINED);
  Operand(0, sr_lr);

  Instruction_Group("b ctr",
        TOP_bctr,
		    TOP_bctrl,
		    TOP_UNDEFINED);
  Operand(0, sr_ctr);
*/

  Instruction_Group("load imm",
	    TOP_li,
	    TOP_lis,   
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, simm16, opnd1);
  
  Instruction_Group("load addr",
	    TOP_la,   
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, simm16, opnd1);
  Operand(1, int32, opnd2);
  
  Instruction_Group("int unary arithmetic",
	    TOP_neg,
	    TOP_extsb,
	    TOP_extsh,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  
  Instruction_Group("int arithmetic",
		    TOP_add,
		    TOP_add_,
		    TOP_subf,
		    TOP_subfe,
		    TOP_addc,
				TOP_addco,
				TOP_adde,
				TOP_addeo,
				TOP_subfc,
				TOP_subfco,
				TOP_subfeo,
		    TOP_and,
		    TOP_andc,
		    TOP_nand,
		    TOP_nor,
		    TOP_or,
		    TOP_xor,
		    TOP_eqv,
		    TOP_orc,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);

  Instruction_Group("int arithmetic compare",
		    TOP_cmpw,
		    TOP_cmplw,
	    TOP_UNDEFINED);
  Result(0, cr); // Operand(0, uimm3);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);

  Instruction_Group("int arithmetic compare immediate",
		    TOP_cmpwi,	
	    TOP_UNDEFINED);
  Result(0, cr); // Operand(0, uimm3);
  Operand(0, int32, opnd1);
  Operand(1, simm16, opnd2);	    


  Instruction_Group("int arithmetic compare logic immediate",
		    TOP_cmplwi,
	           TOP_UNDEFINED);
  Result(0, cr); // Operand(0, uimm3);
  Operand(0, int32, opnd1);
  Operand(1, uimm16, opnd2);	    

  Instruction_Group("int arithmetic with simm",
		    TOP_addi,
        TOP_addis,
		    TOP_addic,
		    TOP_subfic,
		    TOP_mulli,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, simm16, opnd2);


  Instruction_Group("int arithmetic with uimm",
		    TOP_andi_,
		    TOP_ori,
		    TOP_xori,
		    TOP_andis_,
		    TOP_oris,
		    TOP_xoris,		    
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, uimm16, opnd2);

	Instruction_Group("int arithmetic with zero or minus one",
				TOP_addme,
				TOP_addmeo,
				TOP_addze,
				TOP_addzeo,
				TOP_subfme,
				TOP_subfmeo,
				TOP_subfze,
				TOP_subfzeo,
			TOP_UNDEFINED);	
	Result(0, int32);
	Operand(0, int32, opnd1);


  Instruction_Group("int load",
		    TOP_lbz,
		    TOP_lbzu,
		    TOP_lhz,
		    TOP_lhzu,
		    TOP_lha,
		    TOP_lhau,
		    TOP_lwz,
		    TOP_lwzu,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, base);
  Operand(1, simm16, offset);

  Instruction_Group("int store",
		    TOP_stb,
		    TOP_sth,
		    TOP_stw,
		    TOP_stbu,
		    TOP_sthu,
		    TOP_stwu,
	    TOP_UNDEFINED);
  Operand(0, int32, storeval);
  Operand(1, int32, base);
  Operand(2, simm16, offset);

Instruction_Group("int load x",
        TOP_lbzx,
    	TOP_lbzux,
    	TOP_lhzx,
    	TOP_lhzux,
    	TOP_lhax,
    	TOP_lhaux,
    	TOP_lwzx,
    	TOP_lwzux,      	
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, base);
  Operand(1, int32, offset);
  
  Instruction_Group("int store x",		  
    	TOP_stbx,
    	TOP_stbux,
    	TOP_sthx,
    	TOP_sthux,
    	TOP_stwx,
    	TOP_stwux,
	    TOP_UNDEFINED);
  Operand(0, int32, storeval);
  Operand(1, int32, base);
  Operand(2, int32, offset);

  Instruction_Group("int mult/div",
		    TOP_divw,
		    TOP_mullw,
            TOP_mullwo,
            TOP_mulhw,
            TOP_mulhwu,
            TOP_divwo,
            TOP_divwu,
            TOP_divwuo,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);

  Instruction_Group("float compare",
          TOP_fcmpu,
		      TOP_fcmpo,
	    TOP_UNDEFINED);
  Result(0, cr); // Operand(0, uimm3);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);

  Instruction_Group("int move/not",
		    TOP_mr,
		    TOP_not,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  
  Instruction_Group("float moves",
		    TOP_fmr,
	    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);


  Instruction_Group("shifts",
		    TOP_slwi,
		    TOP_srawi,
		    TOP_srwi,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, uimm5, opnd2);

  Instruction_Group("variable shifts",
		    TOP_slw,
		    TOP_sraw,
		    TOP_srw,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);


  Instruction_Group("rotate left imm",
        TOP_rlwinm,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);  
  Operand(1, uimm5, opnd2);
  Operand(2, uimm5, opnd3);
  Operand(3, uimm5, opnd4);
  
  Instruction_Group("rotate left imm insert",
        TOP_rlwimi,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);  
  Operand(1, uimm5, opnd2);
  Operand(2, uimm5, opnd3);
  Operand(3, uimm5, opnd4);
  Operand(4, int32, opnd5);
  		  

  Instruction_Group("rotate left",
		    TOP_rlwnm,            
	    TOP_UNDEFINED);
  Result(0, int32);  
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);
  Operand(2, uimm5, opnd3);
  Operand(3, uimm5, opnd4); 
      

  Instruction_Group("float load",
        TOP_lfs,
		    TOP_lfd,
	    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int32, base);
  Operand(1, simm16, offset);

  Instruction_Group("float store",
		    TOP_stfs,
		    TOP_stfd,
	    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, int32, base);
  Operand(2, simm16, offset);

  Instruction_Group("float load indexed",
		    TOP_lfsx,
		    TOP_lfdx,
	    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int32, base);
  Operand(1, int32, offset);

  Instruction_Group("float store indexed",
		    TOP_stfsx,
		    TOP_stfdx,
	    TOP_UNDEFINED);
  Operand(0, fp64,  storeval);
  Operand(1, int32, base);
  Operand(2, int32, offset);

  Instruction_Group("float arithmetic",
		    TOP_fadds,
		    TOP_fadd,
		    TOP_fdivs,
		    TOP_fdiv,
		    TOP_fmuls,
		    TOP_fmul,
		    TOP_fsubs,
		    TOP_fsub,
	    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);

  Instruction_Group("float unary arithmetic",
		    TOP_fabs,
		    TOP_fnabs,
		    TOP_fneg,
		    TOP_fsqrts,
		    TOP_fsqrt,
		    TOP_fres,
		    TOP_frsqrte,
	    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);

  Instruction_Group("float madd",
		    TOP_fmadds,
		    TOP_fmadd,
		    TOP_fmsubs,
		    TOP_fmsub,
		    TOP_fnmadds,
		    TOP_fnmadd,
		    TOP_fnmsubs,
		    TOP_fnmsub,
	    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);
  Operand(2, fp64, maddend);
  
  Instruction_Group("float select",
        TOP_fsel,
        TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64);
  Operand(1, fp64);
  Operand(2, fp64);

  Instruction_Group("branch compared with zero", // reletive branch          
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

	        TOP_UNDEFINED);
  Operand(0, cr, opnd1); // Operand(0, uimm3);
  Operand(1, pc14, target);
  
  Instruction_Group("branch compared with zero jump to lr, ctr",
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
	        TOP_UNDEFINED);
  Operand(0, cr, opnd1); // Operand(0, uimm3);
  
  Instruction_Group("jump",
		    TOP_ba,
		    TOP_b,
		    TOP_bla,
		    TOP_bl,
	    TOP_UNDEFINED);
  Operand(0, pc26, target);

  Instruction_Group("trap",
		    TOP_tweq,
		    TOP_twge,
		    TOP_twlt,
		    TOP_twne,
	    TOP_UNDEFINED);
  Operand(0, int32, opnd1);
  Operand(1, int32, opnd2);
  Operand(2, uimm16);

  Instruction_Group("trap immediate",
		    TOP_tweqi,
		    TOP_twgei,
		    TOP_twlti,
		    TOP_twnei,
	    TOP_UNDEFINED);
  Operand(0, int32, opnd1);
  Operand(1, simm16, opnd2);
  Operand(2, uimm16);

  Instruction_Group("cfc1",
	    TOP_mcrfs,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, fp64, opnd1);

  Instruction_Group("mflr_ctr_cr",
        TOP_mflr,
        TOP_mfctr,
	    TOP_UNDEFINED);
  Result(0, int32);


  Instruction_Group("mflr_ctr_cr",
      TOP_mfcr,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, cr, opnd1);
  
  Instruction_Group("mfcr_field",
      TOP_mcrf,
	    TOP_UNDEFINED);
  Result(0, cr);
  Operand(0, cr, opnd1);

  
  Instruction_Group("mtlr",
        TOP_mtlr,
        TOP_mtctr,
	    TOP_UNDEFINED);
  Operand(0, int32, opnd1);

  Instruction_Group("move to FPSCR",
	    TOP_mtfsf,
	    TOP_UNDEFINED);

  Operand(0, uimm7, opnd1);
  Operand(1, fp64, opnd1);
  
  Instruction_Group("set FPSCR bit",	    
				TOP_mtfsb0,
				TOP_mtfsb1,
	    TOP_UNDEFINED);

  Operand(0, uimm5, opnd1);

  Instruction_Group("move from FPSCR",	    
				TOP_mffs,
	    TOP_UNDEFINED);

  Operand(0, fp64, opnd1);

  Instruction_Group("CR Logical",
  	                    TOP_crand,
                           TOP_cror,
                           TOP_crxor,
                           TOP_crnand,
                           TOP_crnor,
                           TOP_creqv,
                           TOP_crandc,
                           TOP_crorc,
  	TOP_UNDEFINED);
  Result(0, cr);
  Operand(0, uimm5, opnd1);
  Operand(1, uimm5, opnd2);
  Operand(2, uimm5, opnd3);
  Operand(3, cr, opnd4);
  Operand(4, cr, opnd4);

  Instruction_Group("cvt in float registers",
		    TOP_frsp,
		    TOP_fctiw,
		    TOP_fctiwz,
	    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);

  Instruction_Group("label",
	    TOP_label,
	    TOP_UNDEFINED);
  Operand(0, pcrel16);
  Relocatable(0);

  Instruction_Group("spadjust",
	    TOP_spadjust,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, simm16, opnd2);

  Instruction_Group("simaddi",
	    TOP_simaddi,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, int32, opnd1);
  Operand(1, simm16, opnd2);

  Instruction_Group("intrncall",
		    TOP_intrncall,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, uimm16);	// the intrinsic ID

  Instruction_Group("pregtn tuple",
		    TOP_begin_pregtn,
		    TOP_end_pregtn,
	    TOP_UNDEFINED);
  Result(0, int32);
  Operand(0, uimm16);


  ISA_Operands_End();
  return 0;
}
