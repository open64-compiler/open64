/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
//  Generate ISA registers information
///////////////////////////////////////


//  $Revision: 1.18 $
//  $Date: 05/05/05 17:40:09-07:00 $
//  $Author: gautam@jacinth.keyresearch $
//  $Source: common/targ_info/isa/x8664/SCCS/s.isa_registers.cxx $

#include <stddef.h>
#include "isa_registers_gen.h"
#include "targ_isa_subset.h"

/* The order is assigned to favor the parameter passing. */
static const char* int_reg_names[16] = {
  "%rax", "%rbx", "%rbp", "%rsp", "%rdi", "%rsi", "%rdx", "%rcx",
  "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"
};

static const char* xmm_reg_names[16] = {
  "%xmm0", "%xmm1", "%xmm2",  "%xmm3",  "%xmm4",  "%xmm5",  "%xmm6",  "%xmm7",
  "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15",
};

static const char* x87_reg_names[8] = {
  "%st0", "%st1", "%st2", "%st3", "%st4","%st5","%st6","%st7"
};

static const char* mmx_reg_names[8] = {
  "%mm0", "%mm1", "%mm2", "%mm3", "%mm4", "%mm5", "%mm6", "%mm7"
};


enum { RAX=0, RBX, RBP, RSP, RDI, RSI, RDX, RCX,
       R8, R9, R10, R11, R12, R13, R14, R15 };

static int rax_reg[] = { RAX };
static int rdx_reg[] = { RDX };
static int rcx_reg[] = { RCX };
static int rbp_reg[] = { RBP };
static int rsp_reg[] = { RSP };
static int r11_reg[] = { R11 };
static int m32_8bit_regs[] = {RAX, RBX, RCX, RDX};

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
  ISA_Registers_Begin( "x8664" );

  ISA_REGISTER_CLASS rc_integer = ISA_Register_Class_Create( "integer", 64,  true,  false );
  ISA_REGISTER_CLASS rc_fp      = ISA_Register_Class_Create( "float",   64,  true,  false );
  ISA_REGISTER_CLASS rc_rflags  = ISA_Register_Class_Create( "rflags",  64,  false, false );
  ISA_REGISTER_CLASS rc_rip     = ISA_Register_Class_Create( "rip",     64,  false, false );
  ISA_REGISTER_CLASS rc_x87     = ISA_Register_Class_Create( "x87",    128,  true,  false );
  ISA_REGISTER_CLASS rc_x87_cw  = ISA_Register_Class_Create( "x87_cw",  16,  false, false );
  ISA_REGISTER_CLASS rc_mmx     = ISA_Register_Class_Create( "mmx",     64,  true,  false );
  ISA_Register_Set( rc_integer, 0, 15, "%u", int_reg_names,  All_ISA_Mask() );
  ISA_Register_Set( rc_fp,      0, 15, "%u", xmm_reg_names,  All_ISA_Mask() );
  ISA_Register_Set( rc_rflags,  0, 0,  "%rflags", NULL,      All_ISA_Mask() );
  ISA_Register_Set( rc_rip,     0, 0,  "%rip",    NULL,      All_ISA_Mask() );
  ISA_Register_Set( rc_x87,     0, 7,  "%u", x87_reg_names,  All_ISA_Mask() );
  ISA_Register_Set( rc_x87_cw,  0, 0,  "%%x87_cw",NULL,      All_ISA_Mask() );
  ISA_Register_Set( rc_mmx,     0, 7,  "%u", mmx_reg_names,  All_ISA_Mask() );

  ISA_Register_Subclass_Create("rax", rc_integer, 1, rax_reg, NULL);
  ISA_Register_Subclass_Create("rdx", rc_integer, 1, rdx_reg, NULL);
  ISA_Register_Subclass_Create("rcx", rc_integer, 1, rcx_reg, NULL);
  ISA_Register_Subclass_Create("rbp", rc_integer, 1, rbp_reg, NULL);
  ISA_Register_Subclass_Create("rsp", rc_integer, 1, rsp_reg, NULL);
  ISA_Register_Subclass_Create("r11", rc_integer, 1, r11_reg, NULL);

  ISA_Register_Subclass_Create("m32_8bit_regs", rc_integer, 4, m32_8bit_regs, NULL);

  ISA_Registers_End();
}
