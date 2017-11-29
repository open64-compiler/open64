/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

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


//  
//  Generate ISA registers information
///////////////////////////////////////


//  $Revision: 1.17 $
//  $Date: 2001/03/10 01:17:11 $
//  $Author: mtibuild $
//  $Source: /osprey.src/osprey1.0/common/targ_info/isa/ia64/RCS/isa_registers.cxx,v $

#include <stddef.h>
#include "isa_registers_gen.h"
#include "targ_isa_subset.h"

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

main()
{
  ISA_REGISTER_CLASS
    rc_integer16,
    rc_integer,	/* 32-bit default-size register */
    rc_integer64,
    rc_float, /* 32-bit default-size register */
    rc_float64,
    rc_predicate;

  ISA_Registers_Begin("nvisa");

  rc_integer16 = ISA_Register_Class_Create("integer16", 16, true, false);
  rc_integer = ISA_Register_Class_Create("integer", 32, true, false);
  rc_integer64 = ISA_Register_Class_Create("integer64", 64, true, false);
  rc_float = ISA_Register_Class_Create("float", 32, true, false);
  rc_float64 = ISA_Register_Class_Create("float64", 64, true, false);
  rc_predicate = ISA_Register_Class_Create("predicate", 1, false, false);

  // NVISA has limitless number of virtual registers,
  // so want to pick some large number that will handle any program
  ISA_Register_Set(rc_integer16, 0, 9999, "%%rh%d", NULL, All_ISA_Mask());
  /* integer is one class that gets used the most, so allow more regs for it */
  ISA_Register_Set(rc_integer, 0, 23999, "%%r%d", NULL, All_ISA_Mask());
  ISA_Register_Set(rc_integer64, 0, 23999, "%%rd%d", NULL, All_ISA_Mask());

  ISA_Register_Set(rc_float, 0, 23999, "%%f%d", NULL, All_ISA_Mask());
  ISA_Register_Set(rc_float64, 0, 23999, "%%fd%d", NULL, All_ISA_Mask());

  ISA_Register_Set(rc_predicate, 0, 3999, "%%p%d", NULL, All_ISA_Mask());

  ISA_Registers_End();
  return 0;
}
