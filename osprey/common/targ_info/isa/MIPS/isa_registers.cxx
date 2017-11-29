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
//  Generate ISA registers information
///////////////////////////////////////


//  $Revision: 1.6 $
//  $Date: 2006/03/27 05:54:01 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa_registers.cxx,v $

#include <stddef.h>
#include "isa_registers_gen.h"
#include "targ_isa_subset.h"


static const char *int_reg_names[32] = {
    NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL,   "$gp",  "$sp",  NULL,   NULL,
};

#ifdef TARG_SL
// Coprocessor control registers
static const char *cop_creg_names[32] = {
  "$c0", "$c1", "$c2",  "$c3",  "$c4",  "$c5",  "$c6",  "$c7",
  "$c8",  "$c9",  "$c10", "$c11", "$c12", "$c13", "$c14", "$c15",
  "$c16", "$c17", "$c18", "$c19", "$c20", "$c21", "$c22", "$c23",
  "$c24", "$c25", "$c26", "$c27", "$c28", "$c29", "$c30", "$c31"
};

// Coprocessor bank control registers
static const char *cop_breg_names[32] = {
  "$b0",  "$b1",  "$b2",  "$b3",  "$b4",  "$b5",  "$b6",  "$b7",
  "$b8",  "$b9",  "$b10", "$b11", "$b12", "$b13", "$b14", "$b15",
  "$b16", "$b17", "$b18", "$b19", "$b20", "$b21", "$b22", "$b23",
  "$b24", "$b25", "$b26", "$b27", "$b28", "$b29", "$b30", "$b31"
};

// Coprocessor vector registers
static const char *cop_vreg_names[32] = {
  "$v0",  "$v1",  "$v2",  "$v3",  "$v4",  "$v5",  "$v6",  "$v7",
  "$v8",  "$v9",  "$v10", "$v11", "$v12", "$v13", "$v14", "$v15",
  "$v16", "$v17", "$v18", "$v19", "$v20", "$v21", "$v22", "$v23",
  "$v24", "$v25", "$v26", "$v27", "$v28", "$v29", "$v30", "$v31"
};

// Coprocessor scalar registers
static const char *cop_sreg_names[32] = {
  "$s0",  "$s1",  "$s2",  "$s3",  "$s4",  "$s5",  "$s6",  "$s7",
  "$s8",  "$s9",  "$s10", "$s11", "$s12", "$s13", "$s14", "$s15",
  "$s16", "$s17", "$s18", "$s19", "$s20", "$s21", "$s22", "$s23",
  "$s24", "$s25", "$s26", "$s27", "$s28", "$s29", "$s30", "$s31"
};
// MIPS1 FP registers (even)
static const char *fp_reg_names[16] = {
  "$f0",  "$f2",  "$f4",  "$f6",  "$f8",  "$f10", "$f12", "$f14",
  "$f16", "$f18", "$f20", "$f22", "$f24", "$f26", "$f28", "$f30"
};

// MIPS1 FP registers (odd)
static const char *fp_odd_reg_names[16] = {
  "$f1",  "$f3",  "$f5",  "$f7",  "$f9",  "$f11", "$f13", "$f15",
  "$f17", "$f19", "$f21", "$f23", "$f25", "$f27", "$f29", "$f31"
};

//SL control registers
static const char *control_reg_names[32] = {
  "ja",  "ra",	"$lp0",	"$lp1",	"$lp2",	"$lp3",  "$c6", "$c7",
  "$c8", "$c9", "$c10", "$c11", "$c12", "$c13", "$c14", "$c15",
  "$c16","$c17","$c18", "$c19", "$c20", "$c21", "$c22", "$c23",
  "$c24","$c25","$c26", "$c27", "$c28", "$c29", "$c30", "$c31"
};

// SL special registers
static const char *special_reg_names[32] = {
  "$ar0", "$ar1", "$ar2", "$ar3",  "$ar4", "$ar5", "$ar6", "$ar7",
  "$as0", "$as1", "$as2", "$as3", "$as4", "$as5", "$as6", "$as7",  	
  "$acc0", "$acc1", "$acc2", "$acc3", "hi", "$s21", "$s22", "$s23",
  "$s24", "$s25", "$s26", "$s27", "$s28", "$s29", "$s30", "$s31"
};

//SL loop registers
static const char *loop_reg_names[4] = {
  "$lp0", "$lp1", "$lp2", "$lp3",
};

//SL acc registers
static const char *accum_reg_names[4] = {
  "$acc0", "$acc1", "$acc2", "$acc3",
};

//SL2 acc registers
static const char *c2accum_reg_names[2] = { "", "$c2acc" };

//SL2 acc registers
static const char *c2cond_reg_names[2] = { "", "$c2cond" };

//SL2 mvsel internal register
static const char *c2mvsel_reg_names[2] = { "", "$c2mvsel" };

//SL2 vlcs internal register
static const char *c2vlcs_reg_names[2] = { "", "$c2vlcs" };

//SL2 mov pattern register
static const char *c2movpat_reg_names[2] = { "", "$c2movpat" };

//SL ar registers
static const char *addr_reg_names[8] = {
  "$ar0", "$ar1", "$ar2", "$ar3",
  "$ar4", "$ar5", "$ar6", "$ar7",
};
//SL ar size registers
static const char *addr_size_reg_names[8] = {
   "$as0", "$as1", "$as2", "$as3",
   "$as4", "$as5", "$as6", "$as7",
};
#endif // TARG_SL


static int ISA_Mask(ISA_SUBSET isa)
{
  return 1 << (int)isa;
}


static int All_ISA_Mask(void)
{
  int i;
  int mask = 0;
  for (i = ISA_SUBSET_MIN; i <= ISA_SUBSET_MAX; ++i) {
    mask |= 1 << i;
  }
  return mask;
}


static int Range_ISA_Mask(ISA_SUBSET min_isa, ISA_SUBSET max_isa)
{
  int i;
  int mask = 0;
  for (i = (int)min_isa; i <= (int)max_isa; ++i) {
    mask |= 1 << i;
  }
  return mask;
}


main (int argc, char** argv)
{
  ISA_Registers_Begin("MIPS");

  ISA_REGISTER_CLASS rc_integer = ISA_Register_Class_Create("integer", 32, true, false);
  ISA_REGISTER_CLASS rc_float = ISA_Register_Class_Create("float", 32, true, false);

  ISA_REGISTER_CLASS rc_fcc = ISA_Register_Class_Create("fcc", 1, false, false);

  ISA_REGISTER_CLASS rc_hilo = ISA_Register_Class_Create("hilo", 64, false, false);
#ifdef TARG_SL
  ISA_REGISTER_CLASS rc_cop_creg = ISA_Register_Class_Create("cop_creg", 32, true, false);
  ISA_REGISTER_CLASS rc_cop_breg = ISA_Register_Class_Create("cop_breg", 32, true, false);
  ISA_REGISTER_CLASS rc_cop_vreg = ISA_Register_Class_Create("cop_vreg", 32, true, false);
  ISA_REGISTER_CLASS rc_cop_sreg = ISA_Register_Class_Create("cop_sreg", 32, true, false);
  ISA_REGISTER_CLASS rc_float_odd = ISA_Register_Class_Create("float_odd", 64, true, false);
  // bc2t 
  ISA_REGISTER_CLASS rc_copc = ISA_Register_Class_Create("copc", 1, false, false);
#endif
  //SL control register
  ISA_REGISTER_CLASS rc_control = ISA_Register_Class_Create("control", 32, true, false);

#ifdef TARG_SL
  // SL special registers
  ISA_REGISTER_CLASS rc_special = ISA_Register_Class_Create("special", 32, true, false);

  // SL loop registers
  ISA_REGISTER_CLASS rc_loop = ISA_Register_Class_Create("loop", 4, true, false);
  // SL acc registers
  ISA_REGISTER_CLASS rc_accum = ISA_Register_Class_Create("accum", 4, true, false);

  // SL ar registers
  ISA_REGISTER_CLASS rc_addr = ISA_Register_Class_Create("addr", 8, true, false);
  // SL ar size registers
  ISA_REGISTER_CLASS rc_addr_size = ISA_Register_Class_Create("addr_size", 8, true, false);

#endif // TARG_SL

#ifdef TARG_SL2
  // SL2 acc registers
  ISA_REGISTER_CLASS rc_c2accum = ISA_Register_Class_Create("c2accum", 4, true, false);
  // SL2 acc ctrl registers
  ISA_REGISTER_CLASS rc_c2cond = ISA_Register_Class_Create("c2cond", 4, true, false);
  // SL2 mvsel internal register
  ISA_REGISTER_CLASS rc_c2mvsel = ISA_Register_Class_Create("c2mvsel", 4, true, false);
  // SL2 vlcs internal register
  ISA_REGISTER_CLASS rc_c2vlcs = ISA_Register_Class_Create("c2vlcs", 4, true, false);
  // SL2 mov pat register
  ISA_REGISTER_CLASS rc_c2movpat = ISA_Register_Class_Create("c2movpat", 4, true, false);
#endif


  ISA_Register_Set(rc_integer, 0, 31, "$%u", int_reg_names, All_ISA_Mask());
#ifdef TARG_SL
  ISA_Register_Set(rc_float, 0, 15, "$f%u", fp_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_float_odd, 0, 15,"$f%u", fp_odd_reg_names, All_ISA_Mask());
#else
  ISA_Register_Set(rc_float, 0, 31, "$f%u", NULL, All_ISA_Mask());
#endif
  ISA_Register_Set(rc_fcc, 0, 7, "$fcc%u", NULL, All_ISA_Mask());
  ISA_Register_Set(rc_hilo, 0, 0, "$hilo", NULL, All_ISA_Mask());
#ifdef TARG_SL
  ISA_Register_Set(rc_cop_creg, 0, 31, "%u", cop_creg_names, All_ISA_Mask());
  ISA_Register_Set(rc_cop_breg, 0, 31, "%u", cop_breg_names, All_ISA_Mask());
  ISA_Register_Set(rc_cop_vreg, 0, 31, "%u", cop_vreg_names, All_ISA_Mask());
  ISA_Register_Set(rc_cop_sreg, 0, 31, "%u", cop_sreg_names, All_ISA_Mask());
  ISA_Register_Set(rc_copc, 0, 0, "$copc%u", NULL, All_ISA_Mask());
  ISA_Register_Set(rc_control, 0, 31, "%u", control_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_special, 0, 31, "%u", special_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_loop, 0, 3, "%u", loop_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_accum, 0, 3, "%u", accum_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_addr, 0, 7, "%u", addr_reg_names, All_ISA_Mask());
   ISA_Register_Set(rc_addr_size, 0, 7, "%u", addr_size_reg_names, All_ISA_Mask());
#endif
#ifdef TARG_SL2
  ISA_Register_Set(rc_c2accum, 0, 1, "%u", c2accum_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_c2cond, 0, 1, "%u", c2cond_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_c2mvsel, 0, 1, "%u", c2mvsel_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_c2vlcs, 0, 1, "%u", c2vlcs_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_c2movpat, 0, 1, "%u", c2movpat_reg_names, All_ISA_Mask());
#endif
 
  ISA_Registers_End();
}
