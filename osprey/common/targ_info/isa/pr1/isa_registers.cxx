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


#define NUM_INT_REG    (16)
#define NUM_DSP_REG    (16)
#define NUM_SPEC_REG   (16)


static const char *int_reg_names[NUM_INT_REG] = {
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 0-7
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 8-15
};


#define NELEMS(a) (sizeof(a) / sizeof(*(a)))

static const int dx_regs[] = { 0, 1 };
static const int dy_regs[] = { 2, 3 };
static const int addl_regs[] = { 0, 1, 2, 3 };
static const int pc_regs[] = { 1 };
static const int sp_regs[] = { 2 };
static const int gp_regs[] = { 3 };
static const int ra_regs[] = { 4 };
static const int lc_regs[] = { 5 };
static const int pd_regs[] = { 6, 7 };

main()
{
  ISA_Registers_Begin("pr1");

  ISA_REGISTER_CLASS rc_integer = ISA_Register_Class_Create("integer", 32, true, false);
  ISA_REGISTER_CLASS rc_dsp = ISA_Register_Class_Create("dsp", 32, true, false);
  ISA_REGISTER_CLASS rc_special = ISA_Register_Class_Create("special", 32, true, false);

  ISA_Register_Set(rc_integer, 0, 15, "ir%d", int_reg_names, All_ISA_Mask());
  ISA_Register_Set(rc_dsp, 0, 15, "dr%d", NULL, All_ISA_Mask());
  ISA_Register_Set(rc_special, 0, 15, "sr%d", NULL, All_ISA_Mask());

  // addl can only take reg 0 thru 3
  ISA_Register_Subclass_Create("gpbase", rc_integer, NELEMS(addl_regs), addl_regs, NULL);

  // class of dsp regs
  ISA_Register_Subclass_Create("dx", rc_dsp, NELEMS(dx_regs), dx_regs, NULL);
  ISA_Register_Subclass_Create("dy", rc_dsp, NELEMS(dy_regs), dy_regs, NULL);

  // class of special regs
  ISA_Register_Subclass_Create("sr_sp", rc_special, NELEMS(sp_regs), sp_regs, NULL);
  ISA_Register_Subclass_Create("sr_pc", rc_special, NELEMS(sp_regs), pc_regs, NULL);
  ISA_Register_Subclass_Create("sr_gp", rc_special, NELEMS(sp_regs), gp_regs, NULL);
  ISA_Register_Subclass_Create("sr_ra", rc_special, NELEMS(sp_regs), ra_regs, NULL);
  ISA_Register_Subclass_Create("sr_lc", rc_special, NELEMS(sp_regs), lc_regs, NULL);
  ISA_Register_Subclass_Create("sr_pd", rc_special, NELEMS(sp_regs), pd_regs, NULL);

  ISA_Registers_End();
  return 0;
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
