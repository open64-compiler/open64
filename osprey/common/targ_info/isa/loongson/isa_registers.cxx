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


static int Range_ISA_Mask(ISA_SUBSET min_isa, ISA_SUBSET max_isa)
{
  int i;
  int mask = 0;
  for (i = (int)min_isa; i <= (int)max_isa; ++i) {
    mask |= 1 << i;
  }
  return mask;
}

static const char * int_reg_names[32] = {
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 0-7
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 8-15
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 16-23
  NULL,  NULL,  NULL,  NULL,  "$gp",  "$sp",  NULL,  NULL,  // 24-31
};

static const char * hilo_reg_names[2] = {"$hi", "$lo"};
static const char * fsr_reg_names[1] = {"$31"};


static const int addl_regs[] = { 0 };
static const int ec_regs[] = { 0 };
static const int lc_regs[] = { 0 };
static int num_ar_i_regs = 1; 
static int num_ar_m_regs = 1; 
static int ar_i_regs[1], ar_m_regs[1];;

static void init_ar_regs(void)
{
  int i;
  int n;

  n = 0;
  for (i = 0; i <= 63; ++i) ar_m_regs[n++] = i;
  for (i = 112; i <= 127; ++i) ar_m_regs[n++] = i;
  num_ar_m_regs = n;

  n = 0;
  for (i = 48; i <= 127; ++i) ar_i_regs[n++] = i;
  num_ar_i_regs = n;
}

#define NELEMS(a) (sizeof(a) / sizeof(*(a)))

main()
{
  ISA_REGISTER_CLASS
    rc_integer,
    rc_float,
    rc_predicate,
    rc_branch,
    rc_application,
    rc_control,
    rc_hilo,
    rc_fpsr;

  ISA_Registers_Begin("loongson");

  rc_integer = ISA_Register_Class_Create("integer", 64, true, false);
  rc_float = ISA_Register_Class_Create("float", 64, true, false);
  rc_predicate = ISA_Register_Class_Create("predicate", 1, false, true);
  rc_branch = ISA_Register_Class_Create("branch", 64, false, false);
  rc_application = ISA_Register_Class_Create("application", 64, false, false);
  rc_control = ISA_Register_Class_Create("control", 64, false, false);
  rc_hilo = ISA_Register_Class_Create("hilo", 64, true, false);
  rc_fpsr = ISA_Register_Class_Create("fpsr", 64, true, false);

  ISA_Register_Set(rc_integer, 0, 31, "$%d", int_reg_names, All_ISA_Mask());
	
  ISA_Register_Subclass_Create("addl", rc_integer, 
			       NELEMS(addl_regs), addl_regs, NULL);

  ISA_Register_Set(rc_float, 0, 31, "$f%d", NULL, All_ISA_Mask());

  ISA_Register_Set(rc_predicate, 0, 0, "p%d", NULL, All_ISA_Mask());

  ISA_Register_Set(rc_branch, 0, 0, "b%d", NULL, All_ISA_Mask());

  ISA_Register_Set(rc_application, 0, 0, "ar%d", NULL, All_ISA_Mask());
  
  ISA_Register_Subclass_Create("ar_i", rc_application, 
			       num_ar_i_regs, ar_i_regs, NULL);
  ISA_Register_Subclass_Create("ar_m", rc_application, 
			       num_ar_m_regs, ar_m_regs, NULL);
  ISA_Register_Subclass_Create("ec", rc_application, 
			       NELEMS(ec_regs), ec_regs, NULL);
  ISA_Register_Subclass_Create("lc", rc_application, 
			       NELEMS(lc_regs), lc_regs, NULL);

  ISA_Register_Set(rc_control, 0, 0, "cr%d", NULL, All_ISA_Mask());
  ISA_Register_Set(rc_hilo, 0, 1, "$%d", hilo_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_fpsr, 0, 0, "$%d", fsr_reg_names, All_ISA_Mask());

  ISA_Registers_End();
  return 0;
}
