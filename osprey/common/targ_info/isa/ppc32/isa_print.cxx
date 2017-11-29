/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
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
  ISA_Print_Begin("PPC32");

  Define_Macro("END_GROUP", ";");   // end-of-group marker
  Define_Macro("PREDICATE", "(%s)");    // predicate operand format
  Define_Macro("BEGIN_BUNDLE", ""); // bundle introducer
  Define_Macro("END_BUNDLE", ";");    // bundle terminator

  Set_AsmName_Func(NULL);

  /* One result / two operands */
  ISA_PRINT_TYPE ropop =  ISA_Print_Type_Create("ropop", "%s %s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(ropop,
          TOP_add,
          TOP_add_,
          TOP_addc,
          TOP_addco,
          TOP_adde,
          TOP_addeo,
          TOP_subf,
          TOP_subfc,
          TOP_subfe,
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
          TOP_slw,
          TOP_sraw,
          TOP_srw,
          
          TOP_fadds,
          TOP_fadd,
          TOP_fdivs,
          TOP_fdiv,
          TOP_fmuls,
          TOP_fmul,
          TOP_fsubs,
          TOP_fsub,
          TOP_addi,
          TOP_addis,
          TOP_addic,
          TOP_subfic,

          TOP_cmpw,
          TOP_cmplw,
          TOP_cmpwi,
          TOP_cmplwi,
          TOP_fcmpu,
          TOP_fcmpo,
          
          TOP_andi_,
          TOP_ori,
          TOP_xori,
          TOP_andis_,
          TOP_oris,
          TOP_xoris,
          TOP_slwi,
          TOP_srawi,
          TOP_srwi,
          
          TOP_lbzx,
          TOP_lbzux,
          TOP_lhzx,
          TOP_lhzux,
          TOP_lhax,
          TOP_lhaux,
          TOP_lwzx,
          TOP_lwzux,
        
          TOP_lfsx,
          TOP_lfdx,
        
          TOP_mullw,
          TOP_divw,
          TOP_mulli,
          TOP_mullwo,
          TOP_mulhw,
          TOP_mulhwu,
          TOP_divwo,
          TOP_divwu,
          TOP_divwuo,
          TOP_UNDEFINED);


  /* No result / one operand */
  ISA_PRINT_TYPE op =  ISA_Print_Type_Create("op", "%s %s");
  Name();
  Operand(0);
  Instruction_Print_Group(op,
          TOP_ba,
          TOP_bla,  
          TOP_b,
          TOP_bl,     
          TOP_mtlr,
          TOP_mtctr,  
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
          
          TOP_mtfsb0,
          TOP_mtfsb1,
          TOP_UNDEFINED);

  /* No result / two operands */
  ISA_PRINT_TYPE opop =  ISA_Print_Type_Create("opop", "%s %s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(opop,
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

          TOP_mffs,
        TOP_UNDEFINED);


  /* One result / no operand */
  ISA_PRINT_TYPE r =  ISA_Print_Type_Create("r", "%s %s");
  Name();
  Result(0);
  Instruction_Print_Group(r,
          TOP_mflr,
          TOP_mfctr,
          TOP_mfcr,
        TOP_UNDEFINED);

  /* One result / one operand */
  ISA_PRINT_TYPE rop =  ISA_Print_Type_Create("rop", "%s %s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(rop,
          TOP_fneg,
          TOP_fabs,
          TOP_fnabs,
          TOP_fres,
          TOP_fsqrts,
          TOP_fsqrt,
          TOP_frsqrte,
          TOP_fmr,              
          TOP_frsp,
          TOP_fctiw,
          TOP_fctiwz,
          TOP_addme,
          TOP_addmeo,
          TOP_addze,
          TOP_addzeo,
          TOP_subfme,
          TOP_subfmeo,
          TOP_subfze,
          TOP_subfzeo,
          TOP_extsb,
          TOP_extsh,
        TOP_UNDEFINED);


  /* One operand / one result */
  ISA_PRINT_TYPE opr =  ISA_Print_Type_Create("opr", "%s %s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(opr,
          TOP_mcrf,
          TOP_neg,
          TOP_not,
          TOP_mr,
        TOP_UNDEFINED);


  /* One result / three operands */
  ISA_PRINT_TYPE ropopop =  ISA_Print_Type_Create("ropopop", "%s %s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(ropopop,
          TOP_fmadds,
          TOP_fmadd,
          TOP_fmsubs,
          TOP_fmsub,
          TOP_fnmadds,
          TOP_fnmadd,
          TOP_fnmsubs,
          TOP_fnmsub,
          TOP_fsel,
        TOP_UNDEFINED);
/*
  // five operands
  ISA_PRINT_TYPE rl =  ISA_Print_Type_Create("rl", "%s %s,%s,%s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Operand(4);
  Instruction_Print_Group(rl,
          TOP_rlwimi,
        TOP_UNDEFINED);
*/

  // one result / four operands
  ISA_PRINT_TYPE rl4 =  ISA_Print_Type_Create("rl", "%s %s,%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Operand(3);
  Instruction_Print_Group(rl4,
          TOP_rlwimi,
          TOP_rlwinm,
          TOP_rlwnm,
        TOP_UNDEFINED);
  
  /* No result / three operands */
  ISA_PRINT_TYPE opopop =  ISA_Print_Type_Create("opopop", "%s %s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(opopop,      
          TOP_stbx,
          TOP_stbux,
          TOP_sthx,
          TOP_sthux,
          TOP_stwx,
          TOP_stwux,
          TOP_stfsx,
          TOP_stfdx,  
                  
          TOP_tweq,
          TOP_tweqi,
          TOP_twge,
          TOP_twgei,
          TOP_twlt,
          TOP_twlti,
          TOP_twne,
          TOP_twnei,
          TOP_crand,
          TOP_cror,
          TOP_crxor,
          TOP_crnand,
          TOP_crnor,
          TOP_creqv,
          TOP_crandc,
          TOP_crorc,
        TOP_UNDEFINED);
  
  /* regular load */
  ISA_PRINT_TYPE loadimm =  ISA_Print_Type_Create("load", "%s %s, %s");
  Name();
  Result(0);  
  Operand(0);
  Instruction_Print_Group(loadimm,
          TOP_li,
          TOP_lis,
        TOP_UNDEFINED);
  
  /* regular load */
  ISA_PRINT_TYPE loadaddr =  ISA_Print_Type_Create("load addr", "%s %s, %s(%s)");
  Name();
  Result(0);  
  Operand(0);
  Operand(1);
  Instruction_Print_Group(loadimm,
          TOP_la,       
        TOP_UNDEFINED);
  
  /* regular load */
  ISA_PRINT_TYPE load =  ISA_Print_Type_Create("load", "%s %s,%s(%s)");
  Name();
  Result(0);
  Operand(1);
  Operand(0);
  Instruction_Print_Group(load,
          TOP_lbz,
          TOP_lbzu,
          TOP_lhz,
          TOP_lhzu,
          TOP_lha,
          TOP_lhau,
          TOP_lwz,
          TOP_lwzu,
          TOP_lfs,
          TOP_lfd,
        TOP_UNDEFINED);

  /* regular store */
  ISA_PRINT_TYPE store =  ISA_Print_Type_Create("store", "%s %s,%s(%s)");
  Name();
  Operand(0);
  Operand(2);
  Operand(1);
  Instruction_Print_Group(store,
          TOP_stb,
          TOP_stbu,
          TOP_sth,
          TOP_sthu,
          TOP_stw,
          TOP_stwu,
          TOP_stfs,
          TOP_stfd,
        TOP_UNDEFINED);

  /* No results / no operands TODO */
  ISA_PRINT_TYPE no_rop = ISA_Print_Type_Create("no_rop", "%s");
  Name();
  Instruction_Print_Group(no_rop,
          TOP_nop,
          TOP_sc,
          TOP_blrl,
          TOP_bctrl,
          TOP_blr,
          TOP_bctr,
          TOP_mcrfs,    // the line contains error, only for debug
          TOP_mtfsf,    // the line contains error, only for debug
//        TOP_asm,
//        TOP_intrncall,
//        TOP_spadjust,
          TOP_begin_pregtn,
          TOP_end_pregtn,
          TOP_bwd_bar,
          TOP_fwd_bar,
          TOP_label,
          TOP_noop,
        TOP_UNDEFINED);

  ISA_Print_End();
}
