/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

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
  ISA_Operands_Begin("MIPS");

  /* Literals... */

  OPERAND_VALUE_TYPE simm16, uimm16, uimm1, uimm2, uimm3, uimm5, uimm7, uimm8, uimm9,  \
  uimm10, uimm11, uimm14, uimm15, uid2, uid3, uia1, uia3, uib1, uib3, uic1, uic3, \
  pcrel16, pc26;

  simm16 = ISA_Lit_Opnd_Type_Create("simm16", 16, SIGNED, LC_simm16);
  uimm16 = ISA_Lit_Opnd_Type_Create("uimm16", 16, UNSIGNED, LC_uimm16);
  uimm5 = ISA_Lit_Opnd_Type_Create("uimm5", 5, UNSIGNED, LC_uimm5);
  pcrel16 = ISA_Lit_Opnd_Type_Create("pcrel16", 16, SIGNED, LC_pcrel16);
  pc26 = ISA_Lit_Opnd_Type_Create("pc26", 26, UNSIGNED, LC_pc26);

#ifdef TARG_SL
  OPERAND_VALUE_TYPE uimm4, simm5, pcrel5, simm10, simm9, simm7;
  uimm1 = ISA_Lit_Opnd_Type_Create("uimm1", 1, UNSIGNED, LC_uimm1);  
  uimm2 = ISA_Lit_Opnd_Type_Create("uimm2", 2, UNSIGNED, LC_uimm2);
  uimm3 = ISA_Lit_Opnd_Type_Create("uimm3", 3, UNSIGNED, LC_uimm3);
  uimm4 = ISA_Lit_Opnd_Type_Create("uimm4", 4, UNSIGNED, LC_uimm4);
  uimm7 = ISA_Lit_Opnd_Type_Create("uimm7", 7, UNSIGNED, LC_uimm7);
  uimm8 = ISA_Lit_Opnd_Type_Create("uimm8", 8, UNSIGNED, LC_uimm8);
  uimm9 = ISA_Lit_Opnd_Type_Create("uimm9", 9, UNSIGNED, LC_uimm9);
  uimm10 = ISA_Lit_Opnd_Type_Create("uimm10", 10, UNSIGNED, LC_uimm10);  
  uimm11 = ISA_Lit_Opnd_Type_Create("uimm11", 11, UNSIGNED, LC_uimm11);  
  uimm14 = ISA_Lit_Opnd_Type_Create("uimm14", 14, UNSIGNED, LC_uimm14);  
  uimm15 = ISA_Lit_Opnd_Type_Create("uimm15", 15, UNSIGNED, LC_uimm15);  
  uid2  = ISA_Lit_Opnd_Type_Create("uid2",  2, UNSIGNED, LC_uid2);  
  uid3  = ISA_Lit_Opnd_Type_Create("uid3",  3, UNSIGNED, LC_uid3); 
  uia1  = ISA_Lit_Opnd_Type_Create("uia1",  1, UNSIGNED, LC_uia1); 
  uia3  = ISA_Lit_Opnd_Type_Create("uia3",  3, UNSIGNED, LC_uia3);
  uib1  = ISA_Lit_Opnd_Type_Create("uib1",  1, UNSIGNED, LC_uib1);
  uib3  = ISA_Lit_Opnd_Type_Create("uib3",  3, UNSIGNED, LC_uib3);   
  uic1  = ISA_Lit_Opnd_Type_Create("uic1",  1, UNSIGNED, LC_uic1);
  uic3  = ISA_Lit_Opnd_Type_Create("uic3",  3, UNSIGNED, LC_uic3);     
  simm5 = ISA_Lit_Opnd_Type_Create("simm5", 5, SIGNED, LC_simm5);
  simm7 = ISA_Lit_Opnd_Type_Create("simm7", 7, SIGNED, LC_simm7);
  pcrel5 = ISA_Lit_Opnd_Type_Create("pcrel5",  5,  UNSIGNED, LC_pc5);
  simm9 = ISA_Lit_Opnd_Type_Create("simm9", 9, SIGNED, LC_simm9);
  simm10 = ISA_Lit_Opnd_Type_Create("simm10", 10, SIGNED, LC_simm10);
#endif
  
  /* Registers... */

  OPERAND_VALUE_TYPE int64, fp64, fcc, hilo;

  int64 = ISA_Reg_Opnd_Type_Create("int64", ISA_REGISTER_CLASS_integer,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				64, SIGNED, INVALID);
  fp64 = ISA_Reg_Opnd_Type_Create("fp64", ISA_REGISTER_CLASS_float,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				64, SIGNED, INVALID);
  fcc = ISA_Reg_Opnd_Type_Create("fcc", ISA_REGISTER_CLASS_fcc,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				1, UNSIGNED, INVALID);
  hilo = ISA_Reg_Opnd_Type_Create("hilo", ISA_REGISTER_CLASS_hilo,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				64, SIGNED, INVALID);
#ifdef TARG_SL
  const OPERAND_VALUE_TYPE spereg = 
    ISA_Reg_Opnd_Type_Create("spereg", ISA_REGISTER_CLASS_special,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE accreg = 
    ISA_Reg_Opnd_Type_Create("accreg", ISA_REGISTER_CLASS_accum,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
   const OPERAND_VALUE_TYPE addreg = 
    ISA_Reg_Opnd_Type_Create("addreg", ISA_REGISTER_CLASS_addr,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
    const OPERAND_VALUE_TYPE addsreg = 
    ISA_Reg_Opnd_Type_Create("addsreg", ISA_REGISTER_CLASS_addr_size,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
  /* cop_creg: This is sl2 control register, 
   * different than the following copc
   */
  const OPERAND_VALUE_TYPE cop_creg =
    ISA_Reg_Opnd_Type_Create("creg", ISA_REGISTER_CLASS_cop_creg,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     32, UNSIGNED, INVALID);
  const OPERAND_VALUE_TYPE cop_breg =
    ISA_Reg_Opnd_Type_Create("breg", ISA_REGISTER_CLASS_cop_breg,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     32, UNSIGNED, INVALID);
  const OPERAND_VALUE_TYPE cop_vreg =
    ISA_Reg_Opnd_Type_Create("vreg", ISA_REGISTER_CLASS_cop_vreg,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     64, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE cop_sreg =
    ISA_Reg_Opnd_Type_Create("sreg", ISA_REGISTER_CLASS_cop_sreg,
			     ISA_REGISTER_SUBCLASS_UNDEFINED,
			     64, SIGNED, INVALID);

  /* control register for bc2t to using */
  /* This is the branch condition code register */
  const OPERAND_VALUE_TYPE copc = 
    ISA_Reg_Opnd_Type_Create("copc", ISA_REGISTER_CLASS_copc,
				ISA_REGISTER_SUBCLASS_UNDEFINED,
				1, UNSIGNED, INVALID);
/*SL control register and special register*/
  const OPERAND_VALUE_TYPE creg =
    ISA_Reg_Opnd_Type_Create("creg", ISA_REGISTER_CLASS_control,
                                ISA_REGISTER_SUBCLASS_UNDEFINED,
                                32, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE lpreg =
    ISA_Reg_Opnd_Type_Create("lpreg", ISA_REGISTER_CLASS_loop,
                                ISA_REGISTER_SUBCLASS_UNDEFINED,
                                4, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE acreg =
    ISA_Reg_Opnd_Type_Create("acreg", ISA_REGISTER_CLASS_accum,
                                ISA_REGISTER_SUBCLASS_UNDEFINED,
                                4, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE arreg =
    ISA_Reg_Opnd_Type_Create("arreg", ISA_REGISTER_CLASS_addr,
                                ISA_REGISTER_SUBCLASS_UNDEFINED,
                                8, SIGNED, INVALID);
#endif

#ifdef TARG_SL2
  const OPERAND_VALUE_TYPE c2accreg =
       ISA_Reg_Opnd_Type_Create("c2accreg", ISA_REGISTER_CLASS_c2accum,
                                 ISA_REGISTER_SUBCLASS_UNDEFINED,
                                 1, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE c2cond =
       ISA_Reg_Opnd_Type_Create("c2cond", ISA_REGISTER_CLASS_c2cond,
                                 ISA_REGISTER_SUBCLASS_UNDEFINED,
                                 1, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE c2mvselreg =
       ISA_Reg_Opnd_Type_Create("c2mvselreg", ISA_REGISTER_CLASS_c2mvsel,
                                 ISA_REGISTER_SUBCLASS_UNDEFINED,
                                 1, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE c2vlcsreg =
       ISA_Reg_Opnd_Type_Create("c2vlcsreg", ISA_REGISTER_CLASS_c2vlcs,
                                 ISA_REGISTER_SUBCLASS_UNDEFINED,
                                 1, SIGNED, INVALID);
  const OPERAND_VALUE_TYPE c2movpatreg =
       ISA_Reg_Opnd_Type_Create("c2movpatreg", ISA_REGISTER_CLASS_c2movpat,
                                 ISA_REGISTER_SUBCLASS_UNDEFINED,
                                 1, SIGNED, INVALID);
#endif // TARG_SL2

  /* Enums */
  OPERAND_VALUE_TYPE pfhint;
  pfhint = ISA_Enum_Opnd_Type_Create("pfhint", 8, UNSIGNED, EC_pfhint);

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
#ifdef TARG_SL
          opnd4,
          opnd5,
          opnd6,
          opnd7,
          opnd8,
          opnd9,
          ls_ctrl, 
          lut_idx,
          c2ofst,       // an offset(uimm5)  added to a base used in c2.ld/st
#endif
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
#ifdef TARG_SL 
  opnd4      = Create_Operand_Use("opnd4");
  opnd5      = Create_Operand_Use("opnd5");
  opnd6      = Create_Operand_Use("opnd6");
  opnd7      = Create_Operand_Use("opnd7");
  opnd8      = Create_Operand_Use("opnd8");
  opnd9      = Create_Operand_Use("opnd9");
  ls_ctrl     = Create_Operand_Use("ls_ctrl");
  lut_idx     = Create_Operand_Use("lut_idx");
  c2ofst     = Create_Operand_Use("c2ofst");
#endif
  maddend    = Create_Operand_Use("maddend");
  pos        = Create_Operand_Use("pos");
  width      = Create_Operand_Use("width");

#ifdef TARG_SL
  // new c3
  Instruction_Group("C3-aadda",
                    TOP_c3_aadda,
                    TOP_UNDEFINED);
  Result(0,accreg); // same res
  Operand(0, uimm3);
  Operand(1, accreg);
  Operand(2, uimm1); 
  Operand(3, accreg);

  Instruction_Group("C3-nega",
                    TOP_c3_nega,
                    TOP_UNDEFINED);
  Result(0,accreg); // same res
  Operand(0, uimm3);
  Operand(1, accreg);
  Operand(2, uimm1);

  Instruction_Group("C3-bitr",
                    TOP_c3_bitr,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64);
  Operand(1, uimm5);
  Operand(2, uimm4);

  Instruction_Group("C3-cs",
                    TOP_c3_cs,
                    TOP_UNDEFINED);
  Result(0, int64);
  Result(1, spereg);
  Operand(0, int64);
  Operand(1, int64);
  Operand(2, uimm2);
  Operand(3, spereg);

  Instruction_Group("C3-dadd",
                    TOP_c3_dadd,
                    TOP_c3_dsub,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64);
  Operand(1, int64);
  
  Instruction_Group("C3-dmac",
                    TOP_c3_dmac,
                    TOP_c3_dmacn,
                    TOP_UNDEFINED);
  Result(0, accreg);  // same res
  Result(1, accreg);
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, int64);
  Operand(3, uimm2);
  Operand(4, uimm1);
  Operand(5, uimm1);
  Operand(6, accreg);
  Operand(7, accreg);

  Instruction_Group("C3-dmac-a",
                    TOP_c3_dmac_a,
                    TOP_c3_dmacn_a,
                    TOP_UNDEFINED);
  Result(0, accreg);  // same res
  Result(1, accreg);
  Result(2, addreg);
  Result(3, addreg);
  Operand(0, uimm3);
  Operand(1, addreg, base);
  Operand(2, uimm3);
  Operand(3, addreg, base);
  Operand(4, uimm3);
  Operand(5, uimm1);
  Operand(6, uimm1);
  Operand(7, accreg);
  Operand(8, accreg);

  Instruction_Group("C3-dmula",
                    TOP_c3_dmula,
                    TOP_c3_dmulan,
                    TOP_UNDEFINED); 
  Result(0, accreg);
  Result(1, accreg);
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, int64);
  Operand(3, uimm2);
  Operand(4, uimm1);

  Instruction_Group("C3-dmula-a",
                    TOP_c3_dmula_a,
                    TOP_c3_dmulan_a,
                    TOP_UNDEFINED);
  Result(0, accreg);
  Result(1, accreg);
  Result(2, addreg);
  Result(3, addreg);
  Operand(0, uimm3);
  Operand(1, addreg, base);
  Operand(2, uimm3);
  Operand(3, addreg, base);
  Operand(4, uimm3);
  Operand(5, uimm1);
 
  Instruction_Group("C3-dshll",
                    TOP_c3_dshll_i,
                    TOP_c3_dshrl_i,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64);
  Operand(1, uimm5);
  Operand(2, uimm5);

  Instruction_Group("C3-fixfunc",
                    TOP_c3_ffe,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0,int64,opnd1);
  Operand(1,int64,opnd2);
  Operand(2,uimm5,opnd3);

  Instruction_Group("C3-fftld",
                    TOP_c3_ld,
                    TOP_c3_fftld,
                    TOP_UNDEFINED);
  Result(0, int64);
  Result(1, addreg);
  Operand(0, addreg, base);
  Operand(1, uimm1, offset); // skip
  Operand(2, uimm3);
  Operand(3, uimm3);

  Instruction_Group("C3-fftst",
                    TOP_c3_st,
                    TOP_c3_fftst,
                    TOP_UNDEFINED);
  Result(0, addreg);
  Operand(0, int64, storeval);
  Operand(1, addreg, base);
  Operand(2, uimm1, offset); // skip
  Operand(3, uimm3);
  Operand(4, uimm3);

  Instruction_Group("C3-lead",
                    TOP_c3_lead,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, accreg);

  Instruction_Group("C3-mac",
                    TOP_c3_mac,
                    TOP_c3_macn,
                    TOP_UNDEFINED);
  Result(0, accreg); // same res
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, int64);
  Operand(3, uimm1);
  Operand(4, accreg);
 
  Instruction_Group("C3-mac-a",
                    TOP_c3_mac_a,
                    TOP_c3_macn_a,
                    TOP_UNDEFINED);
  Result(0, accreg); // same res
  Result(1, addreg);
  Result(2, addreg);
  Operand(0, uimm3);
  Operand(1, addreg, base);
  Operand(2, uimm3);
  Operand(3, addreg, base);
  Operand(4, uimm3);
  Operand(5, uimm1);
  Operand(6, accreg);
 

  Instruction_Group("C3-mac-ar",
                    TOP_c3_mac_ar,
                    TOP_c3_macn_ar,
                    TOP_UNDEFINED);
  Result(0, accreg);  // same res
  Result(1, addreg);
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, addreg, base);
  Operand(3, uimm3);
  Operand(4, uimm1);
  Operand(5, accreg);
 
  Instruction_Group("C3-mac-i",
                    TOP_c3_mac_i, 
                    TOP_c3_macn_i, 
                    TOP_UNDEFINED);
  Result(0,  accreg); // same res
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, simm9);
  Operand(3, uimm1);
  Operand(4, accreg);

  Instruction_Group("C3-mula",
                    TOP_c3_mula,
                    TOP_c3_mulan,
                    TOP_UNDEFINED);
  Result(0, accreg);
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, int64);
                             
  Instruction_Group("C3-mula-a",
                    TOP_c3_mula_a,
                    TOP_UNDEFINED); 
  Result(0, accreg);
  Result(1, addreg);
  Result(2, addreg);
  Operand(0, uimm3);
  Operand(1, addreg, base);
  Operand(2, uimm3);
  Operand(3, addreg, base);
  Operand(4, uimm3);

  Instruction_Group("C3-mula-ar",
                    TOP_c3_mula_ar,
                    TOP_UNDEFINED);
  Result(0, accreg);
  Result(1, addreg);
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, addreg, base);
  Operand(3, uimm3);

  Instruction_Group("C3-mula-i",
                    TOP_c3_mula_i,
                    TOP_UNDEFINED);
  Result(0, accreg);
  Operand(0, uimm3);
  Operand(1, int64); 
  Operand(2, simm9);

  Instruction_Group("C3-muls",
                    TOP_c3_muls,
                    TOP_c3_mulus,
                    TOP_UNDEFINED);
  Result(0, spereg);
  Result(1, int64);
  Operand(0,int64,opnd1);
  Operand(1, int64, opnd2);
  Operand(2, uimm5, opnd3);
 
  Instruction_Group("C3-revb",
                    TOP_c3_revb,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64);
  Operand(1, uimm5);

  Instruction_Group("C3-round",
                    TOP_c3_round,
                    TOP_UNDEFINED);
  Result(0, accreg); // same res
  Operand(0, uimm3);
  Operand(1, uimm2);
  Operand(2, uimm1);
  Operand(3, accreg);

  Instruction_Group("C3-saadd-a",
                    TOP_c3_saadd_a,
                    TOP_c3_saaddh_a,
                    TOP_c3_samulh_a,
                    TOP_c3_sasub_a,
                    TOP_c3_sasubh_a,
                    TOP_UNDEFINED);
  Result(0, int64);
  Result(1, addreg);
  Result(2, addreg);
  Operand(0, addreg, base);
  Operand(1, uimm3);
  Operand(2, addreg, base);
  Operand(3, uimm3);
  
  Instruction_Group("C3-saadds",
                    TOP_c3_saadds,
                    TOP_c3_saaddsh,
                    TOP_c3_samulsh,
                    TOP_c3_sasubs,
                    TOP_c3_sasubsh,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64);
  Operand(1, int64);
  Operand(2, uimm5);
 
  Instruction_Group("C3-sadda",
                    TOP_c3_sadda,
                    TOP_UNDEFINED);
  Result(0, accreg);  // same res
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, uimm4);
  Operand(3, uimm2);
  Operand(4, uimm1);
  Operand(5, uimm1);
  Operand(6, accreg);

  Instruction_Group("C3-sadda-a",
                    TOP_c3_sadda_a,
                    TOP_UNDEFINED);
  Result(0, accreg);  // same res
  Result(1, addreg);
  Operand(0, uimm3);
  Operand(1, addreg);
  Operand(2, uimm3);
  Operand(3, uimm4);
  Operand(4, uimm2);
  Operand(5, uimm1);
  Operand(6, uimm1);
  Operand(7, accreg);

  Instruction_Group("C3-shav",
                    TOP_c3_shav,
                    TOP_UNDEFINED);
  Result(0, accreg); // same res
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, uimm1);
  Operand(3, accreg);

  Instruction_Group("C3-shlafa-i",
                    TOP_c3_shlafa_i,
                    TOP_c3_shrafa_i,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, accreg);
  Operand(1, uimm3);
  Operand(2, uimm5);
  
  Instruction_Group("C3-shlata-i",
                    TOP_c3_shlata_i,
                    TOP_c3_shrata_i,
                    TOP_UNDEFINED);
  Result(0, accreg);
  Operand(0, uimm3);
  Operand(1, int64);
  Operand(2, uimm5);
  Operand(3, uimm1);

  Instruction_Group("C3-shra-i",
                    TOP_c3_shla_i,
                    TOP_c3_shra_i,
                    TOP_UNDEFINED);
  Result(0, accreg); // same res
  Operand(0, uimm3);
  Operand(1, uimm5);
  Operand(2, uimm1);
  Operand(3, accreg);
    
  Instruction_Group("C3-subc",
                    TOP_c3_subc,
                    TOP_UNDEFINED);
  Result(0, spereg); //hi
  Result(1, int64);  
  Operand(0, int64);
  Operand(1, int64);
  Operand(2, spereg);

  Instruction_Group("C3-mvts",
                    TOP_c3_mvts,
                    TOP_UNDEFINED);
  Result(0, spereg);
  Operand(0, int64);
  Operand(1, uimm5);

  Instruction_Group("C3-mvfs",
                    TOP_c3_mvfs,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, spereg);
  Operand(1, uimm5);

  Instruction_Group("C3-mvfacc",
                     TOP_c3_mvfacc,
                     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, accreg);
  Operand(1, uimm5);

  Instruction_Group("C3-mvfaddr",
                     TOP_c3_mvfaddr,
                     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, addreg);
  Operand(1, uimm5);

  Instruction_Group("C3-mvfadds",
                     TOP_c3_mvfadds,
                     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, addsreg);
  Operand(1, uimm5);

  Instruction_Group("C3-mvtacc",
                     TOP_c3_mvtacc,
                     TOP_UNDEFINED);
  Result(0, accreg);
  Operand(0, int64);
  Operand(1, uimm5);

  Instruction_Group("C3-mvtaddr",
                     TOP_c3_mvtaddr,
                     TOP_UNDEFINED);
  Result(0, addreg);
  Operand(0, int64);
  Operand(1, uimm5);

  
  Instruction_Group("C3-mvtadds",
                     TOP_c3_mvtadds,
                     TOP_UNDEFINED);
  Result(0, addsreg);
  Result(1, addreg);
  Operand(0, int64);
  Operand(1, uimm5);
  Operand(2, addreg);
  // end new c3

  // core instruction
  Instruction_Group("zero-delay-loop",
                    TOP_loop,
                    TOP_UNDEFINED);
  Operand(0, uimm4);
  Operand(1, pcrel16, target);
  Operand(2, creg, opnd1);
  
  Instruction_Group("ld/st ctrl register",
                    TOP_mvfc,
                    TOP_mvfc16,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0,creg,opnd1);
 
  Instruction_Group("ld/st ctrl register",
                    TOP_mvtc,
                    TOP_mvtc16,
                    TOP_UNDEFINED);
  Result(0, creg);
  Operand(0,int64,opnd1);

  Instruction_Group("mvt loop register",
                    TOP_mvtc_i,
                    TOP_UNDEFINED);
  Result(0, creg);
  Operand(0,uimm10,opnd1);
 
   /*16-bit instr*/
  Instruction_Group("beq/bne",
                    TOP_br16_eqz,
                    TOP_br16_nez,
                    TOP_UNDEFINED);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, pcrel5, target);
   
  //case 1: one result , one operand
  Instruction_Group ("copy and abs",
		     TOP_abs16,
#ifdef TARG_SL2
	            TOP_c2_bcst_q,
	            TOP_c2_bcst_i,
#endif 
		     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0,int64,opnd1);
  
	//case 2: one result , two gpr operands
  Instruction_Group ("int gpr op",
       		     TOP_add16,
		     TOP_and16,
		     TOP_inv16,
		     TOP_or16,
		     TOP_shll16,
		     TOP_shra16,
		     TOP_shrl16,
		     TOP_sub16,
		     TOP_xor16,
                     TOP_mv16,
		     TOP_UNDEFINED);
  Result(0, int64);
  Operand(0,int64,opnd1);
  Operand(1,int64,opnd2);
  
	//case 3: one result , one gpr operand ,one imm5 operand
  Instruction_Group("with uimm5",
            	    TOP_add16_i,
		    TOP_shll16_i,
		    TOP_shrl16_i,
                    TOP_shra16_i,
		    TOP_sub16_i,
		    TOP_and16_i,
                    TOP_or16_i,
                    TOP_xor16_i,
                    TOP_mv16_i,
		    TOP_UNDEFINED);
  Result(0,int64);
  Operand(0,int64,opnd1);
  Operand(1,uimm5,opnd2);
  
  Instruction_Group("with simm5",
	            TOP_add16_sp,
		    TOP_UNDEFINED);
  Result(0,int64);
  Operand(0,int64,opnd1);
  Operand(1,simm7,opnd2);
  
  //case 4: no result  / one operand control register
  Instruction_Group("jump register16",
                    TOP_jr16,
		    TOP_UNDEFINED);
  Operand(0, creg, target);
  
  // case 5: one result control register/ one operand 
  Instruction_Group("jump-and-link register16",
                    TOP_jr16_lnk,
		    TOP_UNDEFINED);
  Result(0, creg);
  Operand(0, creg, target);
	
  //case 6: no result  / no operand 
  Instruction_Group("nop16",
                    TOP_nop16,
		    TOP_UNDEFINED);
  
  //case 7: push pop
  Instruction_Group("lw16",
                    TOP_pop16,
                    TOP_ldw16, 
                    TOP_ldub16_rs,
                    TOP_lduh16_rs,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, base);
  Operand(1, uimm7, offset);
  
  Instruction_Group("st16",
                    TOP_push16,
                    TOP_stw16,
		    TOP_UNDEFINED);
  Operand(0, int64, storeval);
  Operand(1, int64, base);
  Operand(2, uimm7, offset);
  
#endif

  Instruction_Group("no operand",
		    TOP_nop,
		    TOP_noop,
		    TOP_sync,
		    TOP_break,
		    TOP_syscall,
		    TOP_asm,
		    TOP_bwd_bar,
		    TOP_fwd_bar,
#if defined(TARG_SL) || defined(TARG_SL2)		    
                  TOP_peripheral_rw_begin, 
                  TOP_peripheral_rw_end, 
#endif                  
		    TOP_UNDEFINED);

  Instruction_Group("load imm",
		    TOP_lui,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, uimm16, opnd2);

  Instruction_Group("int arithmetic",
		    TOP_add,
		    TOP_addu,
		    TOP_dadd,
		    TOP_daddu,
		    TOP_slt,
		    TOP_sltu,
		    TOP_sub,
		    TOP_subu,
		    TOP_dsub,
		    TOP_dsubu,
		    TOP_and,
		    TOP_nor,
		    TOP_or,
		    TOP_xor,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);

  Instruction_Group("int arithmetic with simm",
		    TOP_addi,
		    TOP_daddi,
		    TOP_addiu,
		    TOP_daddiu,
		    TOP_slti,
		    TOP_sltiu,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, simm16, opnd2);

  Instruction_Group("int arithmetic with uimm",
		    TOP_andi,
		    TOP_ori,
		    TOP_xori,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, uimm16, opnd2);

  Instruction_Group("int load",
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
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, base);
  Operand(1, simm16, offset);

  Instruction_Group("int store",
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
		    TOP_UNDEFINED);
  Operand(0, int64, storeval);
  Operand(1, int64, base);
  Operand(2, simm16, offset);

  Instruction_Group("prefetch",
		    TOP_pref,
		    TOP_prefx,
		    TOP_UNDEFINED);
  Operand(0, pfhint);
  Operand(1, int64, base);
  Operand(2, simm16, offset);

  Instruction_Group("sl5-smult",
		    TOP_smult,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, simm16, opnd2);

  Instruction_Group("int mult/div",
		    TOP_div,
		    TOP_divu,
		    TOP_mult,
		    TOP_multu,
		    TOP_ddiv,
		    TOP_ddivu,
		    TOP_dmult,
		    TOP_dmultu,
		    TOP_UNDEFINED);
  Result(0, hilo);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);

  Instruction_Group("move from hi/lo",
		    TOP_mfhi,
		    TOP_mflo,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, hilo);

  Instruction_Group("move to hi/lo",
		    TOP_mthi,
		    TOP_mtlo,
		    TOP_UNDEFINED);
  Operand(0, int64, opnd1);
  Result(0, hilo);

  Instruction_Group("float compare",
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
		    TOP_UNDEFINED);
  Result(0, fcc);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);

  Instruction_Group("int movf/movt",
		    TOP_movf,
		    TOP_movt,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, fcc, opnd2);

  Instruction_Group("int movn/movz",
		    TOP_movn,
		    TOP_movz,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);

  Instruction_Group("float moves",
		    TOP_mov_s,
		    TOP_mov_d,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);

  Instruction_Group("float movf/movt",
		    TOP_movf_s,
		    TOP_movf_d,
		    TOP_movt_s,
		    TOP_movt_d,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, fcc, opnd2);

  Instruction_Group("float movn/movz",
		    TOP_movn_s,
		    TOP_movn_d,
		    TOP_movz_s,
		    TOP_movz_d,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, int64, opnd2);

  Instruction_Group("shifts",
		    TOP_sll,
		    TOP_dsll,
		    TOP_dsll32,
		    TOP_sra,
		    TOP_dsra,
		    TOP_dsra32,
		    TOP_srl,
		    TOP_dsrl,
		    TOP_dsrl32,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, uimm5, opnd2);

  Instruction_Group("variable shifts",
		    TOP_sllv,
		    TOP_srav,
		    TOP_srlv,
		    TOP_dsllv,
		    TOP_dsrav,
		    TOP_dsrlv,
#ifdef TARG_SL2
	            TOP_c2_sub_g_abs,
#endif 
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);


 Instruction_Group("float load",
		    TOP_lwc1,
		    TOP_ldc1,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int64, base);
  Operand(1, simm16, offset);

  Instruction_Group("float store",
		    TOP_swc1,
		    TOP_sdc1,
		    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, int64, base);
  Operand(2, simm16, offset);

  Instruction_Group("float load indexed",
		    TOP_lwxc1,
		    TOP_ldxc1,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int64, base);
  Operand(1, int64);

  Instruction_Group("float store indexed",
		    TOP_swxc1,
		    TOP_sdxc1,
		    TOP_UNDEFINED);
  Operand(0, fp64, storeval);
  Operand(1, int64, base);
  Operand(2, int64);

  Instruction_Group("float arithmetic",
		    TOP_add_s,
		    TOP_add_d,
		    TOP_div_s,
		    TOP_div_d,
		    TOP_mul_s,
		    TOP_mul_d,
		    TOP_sub_s,
		    TOP_sub_d,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);

  Instruction_Group("float unary arithmetic",
		    TOP_abs_s,
		    TOP_abs_d,
		    TOP_neg_s,
		    TOP_neg_d,
		    TOP_sqrt_s,
		    TOP_sqrt_d,
		    TOP_recip_s,
		    TOP_recip_d,
		    TOP_rsqrt_s,
		    TOP_rsqrt_d,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);

  Instruction_Group("float madd",
		    TOP_madd_s,
		    TOP_madd_d,
		    TOP_msub_s,
		    TOP_msub_d,
		    TOP_nmadd_s,
		    TOP_nmadd_d,
		    TOP_nmsub_s,
		    TOP_nmsub_d,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, fp64, opnd1);
  Operand(1, fp64, opnd2);
  Operand(2, fp64, maddend);

  Instruction_Group("beq/bne",
		    TOP_beq,
		    TOP_bne,
		    TOP_UNDEFINED);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, pcrel16, target);

  Instruction_Group("branch compared with zero",
		    TOP_bgez,
		    TOP_bgtz,
		    TOP_blez,
		    TOP_bltz,
		    TOP_bgezal,
		    TOP_bltzal,
		    TOP_UNDEFINED);
  Operand(0, int64, opnd1);
  Operand(1, pcrel16, target);

  Instruction_Group("branch fcc",
		    TOP_bc1f,
		    TOP_bc1t,
		    TOP_UNDEFINED);
  Operand(0, fcc, opnd1);
  Operand(1, pcrel16, target);

  Instruction_Group("jump",
		    TOP_j,
		    TOP_UNDEFINED);
  Operand(0, pc26, target);

  Instruction_Group("jump register",
		    TOP_jr,
		    TOP_UNDEFINED);
#if defined(TARG_SL)
  Operand(0, creg, target);
#else
  Operand(0, int64, target);
#endif

#if defined(TARG_SL)
  Instruction_Group("jump register",
                     TOP_ret,
                     TOP_ret16,
                     TOP_UNDEFINED);
  Operand(0, creg);
#endif
  Instruction_Group("jump-and-link",
		    TOP_jal,
		    TOP_UNDEFINED);
#if defined(TARG_SL)
  Result(0, creg);
#else
  Result(0, int64);
#endif
  Operand(0, pc26, target);

  Instruction_Group("jump-and-link register",
		    TOP_jalr,
		    TOP_UNDEFINED);
#if defined(TARG_SL)
  Result(0, creg);
  Operand(0, creg, target);
#else
  Result(0, int64);
  Operand(0, int64, target);
#endif
  
  Instruction_Group("trap",
		    TOP_teq,
		    TOP_tge,
		    TOP_tgeu,
		    TOP_tlt,
		    TOP_tltu,
		    TOP_tne,
		    TOP_UNDEFINED);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, uimm16);

  Instruction_Group("trap immediate",
		    TOP_teqi,
		    TOP_tgei,
		    TOP_tgeiu,
		    TOP_tlti,
		    TOP_tltiu,
		    TOP_tnei,
		    TOP_UNDEFINED);
  Operand(0, int64, opnd1);
  Operand(1, simm16, opnd2);
  Operand(2, uimm16);

  Instruction_Group("cfc1",
		    TOP_cfc1,
		    TOP_UNDEFINED);
#if defined(TARG_SL)
  Result(0, int64);
#else
  Result(0, int64);
  Operand(0, fp64, opnd1);
#endif

  Instruction_Group("mfc1",
		    TOP_mfc1,
		    TOP_dmfc1,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, fp64, opnd1);

  Instruction_Group("ctc1",
		    TOP_ctc1,
		    TOP_UNDEFINED);
#if defined(TARG_SL)
  Operand(0, int64, opnd1);
#else
  Result(0, fp64);
  Operand(0, int64, opnd1);
#endif

  Instruction_Group("mtc1",
		    TOP_mtc1,
		    TOP_dmtc1,
		    TOP_UNDEFINED);
  Result(0, fp64);
  Operand(0, int64, opnd1);

#if defined(TARG_SL)
  // conditional move
  Instruction_Group("mc_abs",
                    TOP_mc_abs,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);

  Instruction_Group("sl_mc_zero",
                    TOP_mc_z_eq,
                    TOP_mc_z_ne,
                    TOP_mc_z_gt,
                    TOP_mc_z_ge,
                    TOP_mc_z_lt,
                    TOP_mc_z_le,
                    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, simm16, opnd3);
  Operand(3, int64, opnd4);
 
  Instruction_Group("sl conditional mv",
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
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, simm16, opnd3);

  Instruction_Group("ExtractBit",
		    TOP_extrbs,
		    TOP_extrbu,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, uimm5, pos);
  Operand(2, uimm5, width); 

  Instruction_Group("DepositBit",
		    TOP_depb,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, uimm5, pos);
  Operand(2, uimm5, width);  
  Operand(3, int64, opnd2); //hidden operand, = result

#endif // TARG_SL

#ifdef TARG_SL2
Instruction_Group ( " c2 mvgr immed access",
                 TOP_c2_mvgr_r2g_h_u_i,
                 TOP_c2_mvgr_r2g_h_i,
                 TOP_UNDEFINED);
Result(0, int64);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm4, opnd2);

Instruction_Group ( " c2 mvgr word immed access",
                 TOP_c2_mvgr_r2g_w_i,
                 TOP_UNDEFINED);
Result(0, int64);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm4, opnd3);


Instruction_Group( "c2 clzob",
                  TOP_c2_clzob_zd, 
                  TOP_c2_clzob_od, 
                    TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);

Instruction_Group( "c2 scond indirectly",
	              TOP_c2_scond_eq,
	              TOP_c2_scond_lt,
	              TOP_c2_scond_le,
	              TOP_c2_scond_gt,
	              TOP_c2_scond_ge,
                    TOP_UNDEFINED);
Result(0, int64);
Result(1, c2cond);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);


Instruction_Group( "c2 mvgr word access",
                 TOP_c2_mvgr_r2g_w,
                 TOP_UNDEFINED);
Result(0, int64);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, int64, opnd3);


Instruction_Group( "c2 mvgr access",
                 TOP_c2_mvgr_r2g_h_u,
                 TOP_c2_mvgr_r2g_h,
                 TOP_UNDEFINED);
Result(0, int64);
Operand(0, cop_vreg, opnd1);
Operand(1, int64, opnd2);

Instruction_Group("mvgr g2r.ba",
                 TOP_c2_mvgr_g2r_ba_lh,
                 TOP_c2_mvgr_g2r_ba_hh,
                 TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, int64, opnd1);

Instruction_Group("mvgr g2r.ba.w",
                 TOP_c2_mvgr_g2r_ba_w,
                 TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, int64, opnd1);




Instruction_Group("mvgr g2r lh",
	          TOP_c2_mvgr_g2r_lh,
	          TOP_c2_mvgr_g2r_hh,
	          TOP_c2_mvgr_g2r_b4,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);

Instruction_Group("mvgr g2r word",
	          TOP_c2_mvgr_g2r_w,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);


Instruction_Group("mvgr g2r lh imm",
                TOP_c2_mvgr_g2r_lh_i,
                TOP_c2_mvgr_g2r_hh_i,
                TOP_c2_mvgr_g2r_b4_i,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, int64, opnd1);
Operand(1, uimm4, opnd2);

Instruction_Group("mvgr g2r word imm",
                TOP_c2_mvgr_g2r_w_i,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg); 
Operand(0, int64, opnd1);
Operand(1, uimm4, opnd2);


Instruction_Group("mvgr g2r b",
	         TOP_c2_mvgr_g2r_bh,
	         TOP_c2_mvgr_g2r_bh_u,
	         TOP_c2_mvgr_g2r_bv,
	         TOP_c2_mvgr_g2r_bv_u,

	         TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, int64, opnd1);


Instruction_Group("mvgc c2g",
	         TOP_c2_mvgc_c2g,
	         TOP_UNDEFINED);
Result(0, int64);
Operand(0, cop_creg, opnd1);

Instruction_Group("mvgc g2c",
	         TOP_c2_mvgc_g2c,
	         TOP_UNDEFINED);
Result(0, cop_creg);
Operand(0, int64, opnd1);

Instruction_Group(" ld v_sw",
	         TOP_c2_ld_v_sw,
                 TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, c2movpatreg);
Operand(0, int64, base);
Operand(1, cop_creg, ls_ctrl);

Instruction_Group(" ld vbuf, word mode",
                TOP_c2_ld_v_w,
                TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, int64, base);
Operand(1, cop_creg, ls_ctrl); 


Instruction_Group(" ld vbuf",
	         TOP_c2_ld_v_b_u,
	         TOP_c2_ld_v_b,
	         TOP_c2_ld_v_h,
	         TOP_c2_ld_v_m_b_u,
	         TOP_c2_ld_v_m_b,
	         TOP_c2_ld_v_m_h,
	         TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, int64, base);
Operand(1, cop_creg, ls_ctrl);

Instruction_Group(" ld vbuf macro",
	         TOP_c2_ld_v_m_w,
	         TOP_UNDEFINED); 
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, int64, base);
Operand(1, cop_creg, ls_ctrl);


Instruction_Group(" ld to gpr",
	         TOP_c2_ld_s_h_u_p,
	         TOP_c2_ld_s_h_u,
	         TOP_c2_ld_s_h_p,
	         TOP_c2_ld_s_h,
	         TOP_c2_ld_s_w_p,
	         TOP_c2_ld_s_w,
	         TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, base);

Instruction_Group("c2.ldv2g",
 	         TOP_c2_ld_v2g_b_u,
                 TOP_c2_ld_v2g_b,
                 TOP_c2_ld_v2g_h_u, 
                 TOP_c2_ld_v2g_h,   
                 TOP_c2_ld_v2g_w,   	         
	         TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, base);
Operand(1, uimm5, c2ofst);

Instruction_Group("store to simd register",
               TOP_c2_st_v_w,
               TOP_UNDEFINED);
Operand(0, cop_vreg, storeval);
Operand(1, cop_vreg, storeval);
Operand(2, int64, base);
Operand(3, cop_creg, ls_ctrl);

Instruction_Group("store to simd register",
	        TOP_c2_st_v_b,
	        TOP_c2_st_v_h,
	        TOP_c2_st_v_m_b,
	        TOP_c2_st_v_m_h,
	        TOP_UNDEFINED);
Operand(0, cop_vreg, storeval);
Operand(1, int64, base);
Operand(2, cop_creg, ls_ctrl);


Instruction_Group("store to simd register",
	        TOP_c2_st_v_m_w,
	        TOP_UNDEFINED);
Operand(0, cop_vreg, storeval);
Operand(1, cop_vreg, storeval);
Operand(2, int64, base);
Operand(3, cop_creg, ls_ctrl);


Instruction_Group("store to simd register",
	        TOP_c2_st_s_h,
	        TOP_c2_st_s_h_p,
	        TOP_c2_st_s_w,
	        TOP_c2_st_s_w_p,
	        TOP_UNDEFINED);
Operand(0, int64, storeval);
Operand(1, int64, base);

Instruction_Group("c2.st.g2v",
	        TOP_c2_st_g2v_b, 
                TOP_c2_st_g2v_h, 
                TOP_c2_st_g2v_w, 
	        TOP_UNDEFINED);
Operand(0, int64, storeval);
Operand(1, int64, base);
Operand(2, uimm5, c2ofst); 

Instruction_Group("ld from sbuf to gpr",
	        TOP_c2_ldi_s_h_u,
	        TOP_c2_ldi_s_h,
	        TOP_c2_ldi_s_w,
               TOP_UNDEFINED);
Result(0, int64);
Operand(0, uimm14, offset);

Instruction_Group("ld from sbuf to ctrl reg",
               TOP_c2_ldi_c,
               TOP_UNDEFINED);
Result(0, cop_creg);
Operand(0, uimm14, offset);


Instruction_Group("ld from vbuf to simd reg",
	        TOP_c2_ldi_v_b_u,
	        TOP_c2_ldi_v_b,
	        TOP_c2_ldi_v_h,
	        TOP_c2_ldi_v_m_b_u,
	        TOP_c2_ldi_v_m_b,
	        TOP_c2_ldi_v_m_h,
               TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, uimm11, offset);
Operand(1, cop_creg, ls_ctrl);


Instruction_Group("ld from vbuf to simd reg in word size",
	        TOP_c2_ldi_v_w,
	        TOP_c2_ldi_v_m_w,
               TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, uimm11, offset);
Operand(1, cop_creg, ls_ctrl);

Instruction_Group("ld vbuf to simd reg",
              TOP_c2_ldi_v2g_b_u, 
              TOP_c2_ldi_v2g_b,   
              TOP_c2_ldi_v2g_h_u, 
              TOP_c2_ldi_v2g_h,   
              TOP_c2_ldi_v2g_w,   
               TOP_UNDEFINED);
Result(0, int64);
Operand(0, uimm15, offset);



Instruction_Group(" store to vbuf",
	        TOP_c2_sti_v_b,
	        TOP_c2_sti_v_h,
	        TOP_c2_sti_v_m_b,
	        TOP_c2_sti_v_m_h,
	        TOP_UNDEFINED);
Operand(0, cop_vreg, storeval);
Operand(1, uimm11, offset);
Operand(2, cop_creg, ls_ctrl);

Instruction_Group(" store to vbuf in word size",
	        TOP_c2_sti_v_w,
	        TOP_c2_sti_v_m_w,
	        TOP_UNDEFINED);
Operand(0, cop_vreg, storeval);
Operand(1, cop_vreg, storeval);
Operand(2, uimm11, offset);
Operand(3, cop_creg, ls_ctrl);

Instruction_Group(" store to vbuf",
               TOP_c2_sti_g2v_b,   
               TOP_c2_sti_g2v_h,   
               TOP_c2_sti_g2v_w,   
	        TOP_UNDEFINED);
Operand(0, int64, storeval);
Operand(1, uimm15, offset);


Instruction_Group("store to sbuf",
	        TOP_c2_sti_s_h,
	        TOP_c2_sti_s_w,
	        TOP_UNDEFINED);
Operand(0, int64, storeval);
Operand(1, uimm14, offset);

Instruction_Group("store to ctrl reg",
	        TOP_c2_sti_c,
	        TOP_UNDEFINED);
Operand(0, cop_creg, storeval);
Operand(1, uimm14, offset);

Instruction_Group("simd add shift",
	        TOP_c2_vadds_h,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm3, opnd4);

Instruction_Group("simd add shift word or pair mode",
	        TOP_c2_vadds_w,
	        TOP_c2_vadds_p,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, uimm1, opnd5);
Operand(5, uimm3, opnd6);

Instruction_Group("simd add shift, one ctrl-reg",
	        TOP_c2_vadds_h_mode6,
	        TOP_c2_vadds_h_mode2,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm3, opnd4);
Operand(4, cop_creg);

Instruction_Group("simd add shift word or pair mode, one ctrl-reg",
	        TOP_c2_vadds_w_mode6,
	        TOP_c2_vadds_w_mode2,
	        TOP_c2_vadds_p_mode6,
	        TOP_c2_vadds_p_mode2,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, uimm1, opnd5);
Operand(5, uimm3, opnd6);
Operand(6, cop_creg);

Instruction_Group("simd sub shft",
	        TOP_c2_vsubs_h,
	        TOP_c2_vsubs_h_abs,
                TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm2, opnd4);

/* add the shift_mode&0x2 = 2 */
Instruction_Group("simd sub shft and sm&0x2=2",
	        TOP_c2_vsubs_h_sm,
	        TOP_c2_vsubs_h_abs_sm,
                TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm2, opnd4);
Operand(4, cop_creg);

Instruction_Group("abs half", 
	        TOP_c2_vabs_h,
	        TOP_UNDEFINED);
Result(0,  cop_vreg);
Operand(0, cop_vreg, opnd1);

/* add the shift_mode&0x2 = 2 */
Instruction_Group("abs half sm&0x=2", 
	        TOP_c2_vabs_h_sm,
	        TOP_UNDEFINED);
Result(0,  cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_creg);

Instruction_Group("simd sub word shft", 
	        TOP_c2_vsubs_w,
	        TOP_c2_vsubs_w_abs,	        
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);/* a pseudo opnd : $rs1+1 */
Operand(2, cop_vreg, opnd3);  
Operand(3, cop_vreg, opnd4);  /* a pseudo opnd : $rs2+1 */
Operand(4, uimm1, opnd5);
Operand(5, uimm2, opnd6);

/* add the shift_mode&0x2 = 2 */
Instruction_Group("simd sub word shft sm&0x2=2", 
	        TOP_c2_vsubs_w_sm,
	        TOP_c2_vsubs_w_abs_sm,	        
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);  /* a pseudo opnd : $rs1+1 */
Operand(2, cop_vreg, opnd3);  
Operand(3, cop_vreg, opnd4);  /* a pseudo opnd : $rs2+1 */
Operand(4, uimm1, opnd5);
Operand(5, uimm2, opnd6);
Operand(6, cop_creg);

Instruction_Group("simd abs word", 
	        TOP_c2_vabs_w,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);  /* a pseudo opnd : $rs2+1 */

/* add the shift_mode&0x2 = 2 */
Instruction_Group("simd abs word and sm&0x2=2", 
	        TOP_c2_vabs_w_sm,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);  /* a pseudo opnd : $rs2+1 */
Operand(2, cop_creg);

Instruction_Group("simd pairwise sub shft", 
	        TOP_c2_vsubs_p,
                TOP_c2_vsubs_p_abs,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);  /* a pseudo opnd : $rs1+1 */
Operand(2, cop_vreg, opnd5);  
Operand(3, cop_vreg, opnd6);  /* a pseudo opnd : $rs2+1 */
Operand(4, uimm1, opnd3);
Operand(5, uimm2, opnd4);

/* add the shift_mode&0x2 = 2 */
Instruction_Group("simd pairwise sub shft and sm&0x2=2", 
	        TOP_c2_vsubs_p_sm,
                TOP_c2_vsubs_p_abs_sm,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);  /* a pseudo opnd : $rs1+1 */
Operand(2, cop_vreg, opnd3);  
Operand(3, cop_vreg, opnd4);  /* a pseudo opnd : $rs2+1 */
Operand(4, uimm1, opnd5);
Operand(5, uimm2, opnd6);
Operand(6, cop_creg);

Instruction_Group("simd pairwise abs", 
	        TOP_c2_vabs_p,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);  /* a pseudo opnd : $rs2+1 */

/* add the shift_mode&0x2 = 2 */
Instruction_Group("simd pairwise abs and sm&0x2=2", 
	        TOP_c2_vabs_p_sm,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);          /* a pseudo one: $rd1+1 */
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);  /* a pseudo opnd : $rs2+1 */
Operand(2, cop_creg);

Instruction_Group("simd mul", 
	        TOP_c2_vmul_h,
	        TOP_c2_vneg_h,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);

Instruction_Group("simd mul word size mode", 
	        TOP_c2_vmul_w,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);

Instruction_Group("simd mul", 
	        TOP_c2_vneg_w,
	        TOP_c2_vneg_p,
	        TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);

Instruction_Group("  shft", 
	          TOP_c2_vshr_h,
	          TOP_c2_vshl_h,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm4, opnd2);

Instruction_Group("  shft pair mode or word mode", 
	          TOP_c2_vshr_p,
	          TOP_c2_vshr_w,
	          TOP_c2_vshl_p,
	          TOP_c2_vshl_w,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm4, opnd3);



Instruction_Group("simd clamp",
	          TOP_c2_vclp,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);

Instruction_Group("simd clamp",
	          TOP_c2_vclp_p,
	          TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg);

Instruction_Group(" simd clamp 2", 
	           TOP_c2_vclp_a,
	           TOP_c2_vclp_s,
	           TOP_c2_vclp_n,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);

/* in ISA, vclp.2 only has one Result and two Operands.
 * but it uses its Result as one operand, so add one more Operand
 */
Instruction_Group(" simd vclp 2", 
	           TOP_c2_vclp_2,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);

Instruction_Group(" comp and logic pair mode", 
	           TOP_c2_vclg_p_lt_and,
	           TOP_c2_vclg_p_lt_or,
	           TOP_c2_vclg_p_le_and,
	           TOP_c2_vclg_p_le_or,
	           TOP_c2_vclg_p_eq_and,
	           TOP_c2_vclg_p_eq_or,
	           TOP_c2_vclg_p_ge_and,
	           TOP_c2_vclg_p_ge_or,
	           TOP_c2_vclg_p_gt_and,
	           TOP_c2_vclg_p_gt_or,
	           TOP_c2_vclg_p_and,
	           TOP_c2_vclg_p_or,
	           TOP_c2_vclg_p_le,
	           TOP_c2_vclg_p_eq,
	           TOP_c2_vclg_p_ge,
	           TOP_c2_vclg_p_gt,
                  TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);

Instruction_Group(" comp and logic word size mode", 
	           TOP_c2_vclg_w_lt_and,
	           TOP_c2_vclg_w_lt_or,
	           TOP_c2_vclg_w_le_and,
	           TOP_c2_vclg_w_le_or,
	           TOP_c2_vclg_w_eq_and,
	           TOP_c2_vclg_w_eq_or,
	           TOP_c2_vclg_w_ge_and,
	           TOP_c2_vclg_w_ge_or,
	           TOP_c2_vclg_w_gt_and,
	           TOP_c2_vclg_w_gt_or,
	           TOP_c2_vclg_w_and,
	           TOP_c2_vclg_w_or,
                  TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);


Instruction_Group(" comp and logic word size mode", 
	           TOP_c2_vclg_w_le,
	           TOP_c2_vclg_w_lt,
	           TOP_c2_vclg_w_ge,
	           TOP_c2_vclg_w_gt,
                  TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);




Instruction_Group(" comp and logic", 
	           TOP_c2_vclg_h_lt_and,
	           TOP_c2_vclg_h_lt_or,
	           TOP_c2_vclg_h_le_and,
	           TOP_c2_vclg_h_le_or,
	           TOP_c2_vclg_h_eq_and,
	           TOP_c2_vclg_h_eq_or,
	           TOP_c2_vclg_h_ge_and,
	           TOP_c2_vclg_h_ge_or,
	           TOP_c2_vclg_h_gt_and,
	           TOP_c2_vclg_h_gt_or,
	           TOP_c2_vclg_h_and,
	           TOP_c2_vclg_h_or,
	           TOP_c2_vclg_h_le,
	           TOP_c2_vclg_h_lt,
	           TOP_c2_vclg_h_ge,
	           TOP_c2_vclg_h_gt,
                  TOP_c2_vcmov_h_f,
                  TOP_c2_vcmov_h_t,
                  TOP_c2_vcmpr_h_eq,
                  TOP_c2_vcmpr_h_lt,
                  TOP_c2_vcmpr_h_le, 
                  TOP_c2_vcmpr_h_gt, 
                  TOP_c2_vcmpr_h_ge,
                  TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);

Instruction_Group(" vcmpr word size mode", 
                  TOP_c2_vcmpr_w_eq, 
                  TOP_c2_vcmpr_w_lt, 
                  TOP_c2_vcmpr_w_le, 
                  TOP_c2_vcmpr_w_gt, 
                  TOP_c2_vcmpr_w_ge, 
                  TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);

Instruction_Group(" vcmov word size mode", 
                  TOP_c2_vcmov_w_f,
                  TOP_c2_vcmov_w_t,
                  TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg);

Instruction_Group(" locate zero bank", 
	           TOP_c2_lczero_z,
	           TOP_c2_lczero_nz_fw,
	           TOP_c2_lczero_nz_bw,
	           TOP_c2_vcopy,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);


Instruction_Group(" simd round",
	           TOP_c2_vrnd_h,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm3, opnd2);

Instruction_Group(" simd round word size mode",
	           TOP_c2_vrnd_w,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm3, opnd3);


Instruction_Group(" simd spas",
	           TOP_c2_vspas,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm5, opnd3);
Operand(3, uimm2, opnd4);


Instruction_Group("simd spel",
      TOP_c2_vspel_mul_h,
      TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm9, lut_idx);

Instruction_Group("simd spel adds",
      TOP_c2_vspel_adds,
      TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm1, opnd2);
Operand(2, uimm9, lut_idx);

Instruction_Group("simd spel word mode",
      TOP_c2_vspel_mul_w,
      TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm9, lut_idx);


Instruction_Group( "spel mac",
     TOP_c2_vspel_mac_h,
      TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm3, opnd2);
Operand(2, uimm9, lut_idx);

Instruction_Group( "spel mac word mode",
     TOP_c2_vspel_mac_w,
      TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, uimm9, lut_idx);


Instruction_Group("mmul.h",
	           TOP_c2_mmul_h,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, uimm4, opnd4);

Instruction_Group("mmul.w",
	           TOP_c2_mmul_w,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, uimm3, opnd5);
Operand(5, uimm4, opnd6);


Instruction_Group("vmov", 
	           TOP_c2_vmov,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm7, opnd2);

Instruction_Group("vmov_swin", 
	           TOP_c2_vmov_swin,
	           TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm1, opnd2);
Operand(2, uimm7, opnd3);
Operand(3, c2movpatreg);


Instruction_Group("sad", 
	            TOP_c2_sad, 
	            TOP_UNDEFINED);
Result(0, int64);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);

Instruction_Group("satd", 
	            TOP_c2_satd,
	            TOP_UNDEFINED);
Result(0, int64);
Result(1, cop_vreg); 
Result(2, cop_vreg); 
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm3, opnd3);


Instruction_Group("intra",
	            TOP_c2_intra,
                   TOP_c2_intra_0_1_9_14_16,
                   TOP_c2_intra_2_3_8_10,
                   TOP_c2_intra_4,
                   TOP_c2_intra_5_11,
                   TOP_c2_intra_6,
                   TOP_c2_intra_7,
                   TOP_c2_intra_12_13,
                   TOP_c2_intra_15_17,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm5, opnd2);

Instruction_Group("mvsel mode0",
	            TOP_c2_mvsel_mode0,
	            TOP_UNDEFINED);
Result(0, int64);
Result(1, c2mvselreg);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, cop_creg, opnd4); 

Instruction_Group("mvsel mode1",
	            TOP_c2_mvsel_mode1,
	            TOP_UNDEFINED);
Result(0, int64);
Result(1, c2mvselreg);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, c2mvselreg);


Instruction_Group("mvsel mode2",
	            TOP_c2_mvsel_mode2,
	            TOP_UNDEFINED);
Result(0, int64);
Result(1, c2mvselreg);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, c2mvselreg, opnd5);
Operand(4, cop_creg, opnd4); 


Instruction_Group("mvsel mode345",
	            TOP_c2_mvsel_mode345,
	            TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, c2mvselreg);


Instruction_Group("c2.mov.g",
	            TOP_c2_mov_g,
	            TOP_UNDEFINED);
Result(0, int64);
Result(1, c2accreg);
Operand(0, int64, opnd1);

Instruction_Group("c2.mov.r",
	             TOP_c2_mov_r,
	             TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_creg);

Instruction_Group("vlcs",
	             TOP_c2_vlcs_dc,
	             TOP_c2_vlcs_ac,
	             TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Result(2, c2vlcsreg);
Operand(0, cop_vreg, opnd1);
Operand(1, c2vlcsreg);

Instruction_Group("vlcs",
	             TOP_c2_vlcs_wb,
	             TOP_UNDEFINED);
Result(0,  int64);
Operand(0, c2vlcsreg);

Instruction_Group("adds.g.i",
                     TOP_c2_add_shl_g_i,
                     TOP_c2_add_shr_g_i,
                     TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm1, opnd4);
Operand(4, uimm2,  opnd5);

Instruction_Group("adds.g",
                    TOP_c2_add_shl_g,
                    TOP_c2_add_shr_g,
	              TOP_UNDEFINED);
Result(0,  int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm1, opnd4);
Operand(4, uimm2, opnd5);

Instruction_Group("adds.r.h.i",
                    TOP_c2_add_shl_r_h_i,
                    TOP_c2_add_shr_r_h_i,
	              TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm1, opnd4);
Operand(4, uimm2, opnd5);
Operand(5, cop_creg);

/* c2.adds.r.w.i, has one more result, and one more source */
Instruction_Group("adds.r.w.i",
                    TOP_c2_add_shl_r_w_i,
                    TOP_c2_add_shr_r_w_i,
	              TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2); 
Operand(2, uimm5, opnd2);
Operand(3, uimm1, opnd3);
Operand(4, uimm1, opnd4);
Operand(5, uimm2, opnd5);
Operand(6, cop_creg);

Instruction_Group("adds.r.h",
                   TOP_c2_add_shl_r_h,
                   TOP_c2_add_shr_r_h,
	             TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm1, opnd3);
Operand(3, uimm1, opnd4);
Operand(4, uimm2, opnd5);
Operand(5, cop_creg);

/* c2.adds.r.w has one more result, and two more sources*/
Instruction_Group("adds.r.w",
                   TOP_c2_add_shl_r_w,
                   TOP_c2_add_shr_r_w,
	             TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, uimm1, opnd3);
Operand(5, uimm1, opnd4);
Operand(6, uimm2, opnd5);
Operand(7, cop_creg);

Instruction_Group("sub.g.[abs].i",
	            TOP_c2_sub_g_abs_i,
	            TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);

Instruction_Group("subs.g.i",
	            TOP_c2_subs_g_i,
	            TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, uimm3, opnd3);


  Instruction_Group("subs.g",
	            TOP_c2_subs_g,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, int64, opnd2);
  Operand(2, uimm3, opnd3);

  



Instruction_Group("bop",
	            TOP_c2_bop_ls,
	            TOP_c2_bop_rs,
	            TOP_c2_bop_and,
	            TOP_c2_bop_or,
	            TOP_c2_bop_xor,
	            TOP_c2_bop_andxor,
	            TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);

Instruction_Group("sub.r.abs.h.i",
	            TOP_c2_sub_r_abs_h_i,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, cop_creg);

/* c2.sub.r.abs.w.r needs one more result and one more source opnd */
Instruction_Group("sub.r.abs.w.i",
	            TOP_c2_sub_r_abs_w_i,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm5, opnd2);
Operand(3, cop_creg);


Instruction_Group("subs.r.h.i",
	            TOP_c2_subs_r_h_i,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, cop_creg);

/* c2.subs.r.w.i needs one more result and one more source opnd*/
Instruction_Group("subs.r.w.i",
	            TOP_c2_subs_r_w_i,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm5, opnd3);
Operand(3, uimm3, opnd4);
Operand(4, cop_creg);


Instruction_Group("mov.s.i",
	            TOP_c2_mov_s_i,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm4, opnd2);
Operand(2, uimm4, opnd3);

Instruction_Group("mov.s",
	            TOP_c2_mov_s,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, int64, opnd2);
Operand(2, int64, opnd3);

Instruction_Group("sub.r.abs.h",
	            TOP_c2_sub_r_abs_h,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_creg);

/* c2.sub.r.abs.w needs one more result, and two more sources */
Instruction_Group("sub.r.abs.w",
	            TOP_c2_sub_r_abs_w,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, cop_creg, opnd5);

Instruction_Group("subs.r.h",
	            TOP_c2_subs_r_h,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm3, opnd3);
Operand(3, cop_creg, opnd4);

/* c2.sub.r.abs.w needs one more result, and two more sources */
Instruction_Group("subs.r.w",
	            TOP_c2_subs_r_w,
	            TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, cop_vreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, uimm3, opnd5);
Operand(5, cop_creg, opnd6);

Instruction_Group("c2.muls",
	            TOP_c2_muls,
	            TOP_UNDEFINED);
Result(0, int64);
Result(1, cop_creg);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, uimm2, opnd3);
Operand(3, int64,  opnd4);
Operand(4, uimm1, opnd5);

Instruction_Group("c2.mads",
	            TOP_c2_mads,
	            TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, int64, opnd3);
Operand(3, uimm1, opnd4);
Operand(4, uimm2, opnd5);

Instruction_Group(" c2.smads",
	             TOP_c2_smads,
	             TOP_c2_med,
	             TOP_c2_bxtr_u_l,
	             TOP_c2_bxtr_s_l,
	             TOP_c2_bxtr_u_m,
	             TOP_c2_bxtr_s_m,	             
	             TOP_c2_cmov,		             
	             TOP_c2_clp,
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, int64, opnd3);

Instruction_Group(" c2.bxtrr48",
	             TOP_c2_bxtrr48,	             
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, cop_vreg, opnd1);
Operand(1, int64, opnd2);
Operand(2, int64, opnd3);

Instruction_Group(" c2.bxtrr48.i",
	             TOP_c2_bxtrr48_i,	             
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, int64, opnd3);

Instruction_Group(" c2.sum4.saddr",
                    TOP_c2_sum4_saddr, 	
	             TOP_UNDEFINED);
Result(0, int64);
Result(1, c2accreg);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, int64, opnd3);

                    


Instruction_Group(" c2.clp.i",
	             TOP_c2_clp_i,
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, int64, opnd3);

Instruction_Group(" c2.bdep.l",
	             TOP_c2_bdep_l,
	             TOP_c2_bdep_m,
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, int64, opnd3);

Instruction_Group("c2.min",
	             TOP_c2_min,
	             TOP_c2_max,
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);

Instruction_Group("c2.mov.c.i",
	              TOP_c2_mov_c_i,
	              TOP_UNDEFINED);
Result(0, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm4, opnd2);

Instruction_Group("c2.mov.c",
	              TOP_c2_mov_c,
	              TOP_UNDEFINED);
Result(0, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, int64, opnd2); 
Operand(2, cop_creg);

Instruction_Group("c2.chkrng",
	              TOP_c2_chkrng,
	              TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, uimm5, opnd3);

Instruction_Group("c2.scond.eq",
                 TOP_c2_scond_eq_i,
                 TOP_c2_scond_lt_i,
                 TOP_c2_scond_le_i,
                 TOP_c2_scond_gt_i, 
                 TOP_c2_scond_ge_i, 
                 TOP_UNDEFINED);
Result(0, int64);
Result(1, c2cond);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);

Instruction_Group("c2.scond.eq",
                 TOP_c2_clzob_zd_i, 
                 TOP_c2_clzob_od_i, 
                 TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);

Instruction_Group("c2.scond.r.eq.i",
                    TOP_c2_scond_r_h_eq,
                    TOP_c2_scond_r_h_lt,
                    TOP_c2_scond_r_h_le,
                    TOP_c2_scond_r_h_gt,
                    TOP_c2_scond_r_h_ge,
                    TOP_UNDEFINED);
Result(0, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_creg, opnd3); //read add_ctrl 

/* word size and non-immed mode need two more source operands */
Instruction_Group("c2.scond.r.w, word size mode",
                    TOP_c2_scond_r_w_eq,
                    TOP_c2_scond_r_w_lt,
                    TOP_c2_scond_r_w_le,
                    TOP_c2_scond_r_w_gt,
                    TOP_c2_scond_r_w_ge,
                    TOP_UNDEFINED);
Result(0, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, cop_creg, opnd5); //read add_ctrl 

Instruction_Group("c2.scond.r.eq",
	              TOP_c2_scond_r_h_eq_i,
	              TOP_c2_scond_r_h_lt_i,
	              TOP_c2_scond_r_h_le_i,
	              TOP_c2_scond_r_h_gt_i,
	              TOP_c2_scond_r_h_ge_i,
	              TOP_UNDEFINED);
Result(0, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, cop_creg, opnd3);

/* word size and immed mode, needs one more rs1+1 operand */
Instruction_Group("c2.scond.r.w immed",
	              TOP_c2_scond_r_w_eq_i,
	              TOP_c2_scond_r_w_lt_i,
	              TOP_c2_scond_r_w_le_i,
	              TOP_c2_scond_r_w_gt_i,
	              TOP_c2_scond_r_w_ge_i,
	              TOP_UNDEFINED);
Result(0, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm5, opnd3);
Operand(3, cop_creg, opnd4); //read add_ctrl 



Instruction_Group("c2.scond.rw.eq",
	              TOP_c2_scond_r_wb_eq_i,
	              TOP_c2_scond_r_wb_lt_i,
	              TOP_c2_scond_r_wb_le_i,
	              TOP_c2_scond_r_wb_gt_i,
	              TOP_c2_scond_r_wb_ge_i,
	              TOP_UNDEFINED);
Result(0, int64);
Result(1, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm5, opnd2);
Operand(2, cop_creg, opnd3); //read add_ctrl

/* word size and immed mode, needs one more rs1+1 operand */
Instruction_Group("c2.scond.rw.eq i",
	              TOP_c2_scond_r_w_wb_eq_i,
	              TOP_c2_scond_r_w_wb_lt_i,
	              TOP_c2_scond_r_w_wb_le_i,
	              TOP_c2_scond_r_w_wb_gt_i,
	              TOP_c2_scond_r_w_wb_ge_i,
	              TOP_UNDEFINED);
Result(0, int64);
Result(1, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, uimm5, opnd3);
Operand(3, cop_creg, opnd4);


Instruction_Group("c2.scond.rw.eq",
                     TOP_c2_scond_r_h_wb_eq,
	              TOP_c2_scond_r_h_wb_lt,
	              TOP_c2_scond_r_h_wb_le,
	              TOP_c2_scond_r_h_wb_gt,
	              TOP_c2_scond_r_h_wb_ge,
	              TOP_UNDEFINED);
Result(0, int64);
Result(1, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_creg, opnd3); 

/* word size and non-immed mode, needs two more source operands */
Instruction_Group("c2.scond.rw.eq.i",
                     TOP_c2_scond_r_w_wb_eq,
	              TOP_c2_scond_r_w_wb_lt,
	              TOP_c2_scond_r_w_wb_le,
	              TOP_c2_scond_r_w_wb_gt,
	              TOP_c2_scond_r_w_wb_ge,
	              TOP_UNDEFINED);
Result(0, int64);
Result(1, c2cond);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, cop_vreg, opnd4);
Operand(4, cop_creg, opnd5);

Instruction_Group("c2.bop.ls.i",
	             TOP_c2_bop_ls_i,
	             TOP_c2_bop_rs_i,
	             TOP_c2_bop_and_i,
	             TOP_c2_bop_or_i,
	             TOP_c2_bop_xor_i,
	             TOP_UNDEFINED);

Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, uimm5, opnd2);

Instruction_Group("c2.sum4.c",
	             TOP_c2_sum4_c,
	             TOP_UNDEFINED);
Result(0, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm4, opnd2);


Instruction_Group("c2.sum4.g",
	             TOP_c2_sum4_g,
	             TOP_c2_sum4_sw,
	             TOP_UNDEFINED);
Result(0, int64);
Result(1, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, uimm4, opnd2);


Instruction_Group("c2.sum4.r",
	              TOP_c2_sum4_r,
	              TOP_UNDEFINED);
Result(0, cop_vreg);
Result(1, c2accreg);
Operand(0, cop_vreg, opnd1);
Operand(1, cop_vreg, opnd2);
Operand(2, cop_vreg, opnd3);
Operand(3, c2accreg, opnd4);
Operand(4, cop_creg );

Instruction_Group("c2.gsums",
	             TOP_c2_gsums,
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);
Operand(2, uimm5, opnd3);
Operand(3, uimm3, opnd4);

Instruction_Group("c2.wrap",
	             TOP_c2_wrap,
	             TOP_UNDEFINED);
Result(0, int64);
Operand(0, int64, opnd1);
Operand(1, int64, opnd2);

Instruction_Group("sl2 fork", 
	           TOP_c2_fork_m,
	           TOP_c2_fork_n,
	           TOP_UNDEFINED);
// the pcrel16 is not correct, we need use pcrel22 for 22bit offset, leave the 
// work to the future. 
Operand(0, pcrel16, opnd1);

Instruction_Group("sl2 joint", 
	           TOP_c2_joint,
                   TOP_c2_thctrl_lock,
                   TOP_c2_thctrl_unlock,
                   TOP_c2_thctrl_deact,
	           TOP_UNDEFINED);

Instruction_Group("c2.thctrl.act", 
                   TOP_c2_thctrl_act,
                   TOP_c2_thctrl_mode4, 
                   TOP_c2_thctrl_mode5,
                   TOP_c2_thctrl_mode6,
                   TOP_c2_thctrl_mode7,
	           TOP_UNDEFINED);
Operand(0, uimm2, opnd1);


Instruction_Group("macro op", 
                  TOP_c2_macro, 
                  TOP_UNDEFINED); 


Instruction_Group("c2 shf alu immed",
                  TOP_c2_shor_l_i,
                  TOP_c2_shor_rl_i,
                  TOP_c2_shor_ra_i,
                  TOP_c2_shadd_l_i,
                  TOP_c2_shadd_rl_i,
                  TOP_c2_shadd_ra_i,
                  TOP_c2_shsub_l_i,
                  TOP_c2_shsub_rl_i, 
                  TOP_c2_shsub_ra_i, 
                  TOP_UNDEFINED); 
Result(0, int64); 
Operand(0, int64); 
Operand(1, uimm5); 
Operand(2, int64); 

Instruction_Group("c2 shf alu op",
                  TOP_c2_shor_l,
                  TOP_c2_shor_rl,
                  TOP_c2_shor_ra,
                  TOP_c2_shadd_l,
                  TOP_c2_shadd_rl,
                  TOP_c2_shadd_ra,
                  TOP_c2_shsub_l,
                  TOP_c2_shsub_rl, 
                  TOP_c2_shsub_ra, 
                  TOP_UNDEFINED); 
Result(0, int64); 
Operand(0, int64); 
Operand(1, int64); 
Operand(2, int64); 


#endif //TARG_SL2

  Instruction_Group("cvt in float registers",
		    TOP_cvt_l_s,
		    TOP_cvt_l_d,
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
		    TOP_cvt_d_s,
		    TOP_cvt_s_d,
		    TOP_cvt_d_w,
		    TOP_cvt_d_l,
		    TOP_cvt_s_w,
		    TOP_cvt_s_l,
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
  Result(0, int64);
  Operand(0, int64, opnd1);
  Operand(1, simm16, opnd2);

  Instruction_Group("auxbr",
                    TOP_auxbr,
                    TOP_UNDEFINED);
  Operand(0, pc26, target);
  

  Instruction_Group("intrncall",
		    TOP_intrncall,
		    TOP_UNDEFINED);
  Result(0, int64);
  Operand(0, uimm16);	// the intrinsic ID

  Instruction_Group("pregtn tuple",
		    TOP_begin_pregtn,
		    TOP_end_pregtn,
		    TOP_UNDEFINED);
  Operand(0, int64);
  Operand(1, uimm16);


  ISA_Operands_End();
  return 0;
}
