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
//  Generate ISA registers information
///////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2006/10/21 19:00:00 $
//  $Author: 
//  $Source: /open64/kpro64/common/targ_info/isa/ppc32/isa_registers.cxx,v $

#include <stddef.h>
#include "isa_registers_gen.h"
#include "targ_isa_subset.h"

// General-Purpose Register
static const char *int_reg_names[32] = {
    "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7", 
    "8",  "9",  "10", "11", "12", "13", "14", "15", 
    "16", "17", "18", "19", "20", "21", "22", "23", 
    "24", "25", "26", "27", "28", "29", "30", "31" 
};


// Floating-Point Registers
static const char *fp_reg_names[32] = {
    "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7", 
    "8",  "9",  "10", "11", "12", "13", "14", "15", 
    "16", "17", "18", "19", "20", "21", "22", "23", 
    "24", "25", "26", "27", "28", "29", "30", "31" 
};

// treat condition regsiters as a special register
// Condition Registers
static const char *cr_reg_names[8] = {
    "0", "1", "2", "3", 
    "4", "5", "6", "7"
};

// Special Purpose Register
static const char *sp_reg_name[5] = {
    "fpscr", "xer", "lr", "ctr",  "cr"
};

static const int fpscr_regs[] = { 1 };
static const int xer_regs[]   = { 2 };
static const int lr_regs[]    = { 3 };
static const int ctr_regs[]   = { 4 };
static const int cr_regs[]    = { 5 };

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

// General-Purpose Regisgers (s) * 32
// Floating-Point  Registers (s) * 32
// Condition Register (CR) * 8
// Floating-Point Status and Control Register (FPSCR)
// XER Regisger (XER)
// Link Register (LR)
// Count Regisger (CTR)
main (int argc, char** argv)
{
  ISA_Registers_Begin("PPC32");

  ISA_REGISTER_CLASS rc_integer     = ISA_Register_Class_Create("integer",   32, true,  false);
  ISA_REGISTER_CLASS rc_float       = ISA_Register_Class_Create("float",     64, true,  false);
  ISA_REGISTER_CLASS rc_condition   = ISA_Register_Class_Create("condition", 4,  true,  false);
  ISA_REGISTER_CLASS rc_special     = ISA_Register_Class_Create("special",   32, false, false);

  ISA_Register_Set(rc_integer,   0, 31, "%d", int_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_float,     0, 31, "%d", fp_reg_names,  All_ISA_Mask());
  ISA_Register_Set(rc_condition, 0, 7,  "%d",  cr_reg_names,  All_ISA_Mask());
  ISA_Register_Set(rc_special,   0, 4,  "sr%d",  sp_reg_name,   All_ISA_Mask());

  // class of special regs  
  ISA_Register_Subclass_Create("sr_fpscr", rc_special, 1, fpscr_regs, NULL);
  ISA_Register_Subclass_Create("sr_xer",   rc_special, 1, xer_regs, NULL);
  ISA_Register_Subclass_Create("sr_lr",    rc_special, 1, lr_regs, NULL);
  ISA_Register_Subclass_Create("sr_ctr",   rc_special, 1, ctr_regs, NULL);
  ISA_Register_Subclass_Create("sr_cr",   rc_special, 1, cr_regs, NULL);
  
  ISA_Registers_End();
}
