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


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/isa/ia64/isa_registers.cxx,v $

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


static const char *ar_reg_names[128] = {
  "ar.k0",     "ar.k1",     "ar.k2",       "ar.k3",		// 0-3
  "ar.k4",     "ar.k5",     "ar.k6",       "ar.k7",		// 4-7
  NULL,        NULL,        NULL,          NULL,		// 8-11
  NULL,        NULL,        NULL,          NULL,		// 12-15
  "ar.rsc",    "ar.bsp",    "ar.bspstore", "ar.rnat",		// 16-19
  NULL,        "ar.fcr",    NULL,          NULL,		// 20-23
  "ar,eflag",  "ar.csd",    "ar.ssd",      "ar.cflg",		// 24-27
  "ar.fsr",    "ar.fir",    "ar.fdr",      NULL,		// 28-31
  "ar.ccv",    NULL,        NULL,          NULL,		// 32-35
  "ar.unat",   NULL,        NULL,          NULL,		// 36-39
  "ar.fpsr",   NULL,        NULL,          NULL,		// 40-43
  "ar.itc",    NULL,        NULL,          NULL,		// 44-47
  NULL,        NULL,        NULL,          NULL,		// 48-51
  NULL,        NULL,        NULL,          NULL,		// 52-55
  NULL,        NULL,        NULL,          NULL,		// 56-59
  NULL,        NULL,        NULL,          NULL,		// 60-63
  "ar.pfs",    "ar.lc",     "ar.ec",       NULL,		// 64-67
  NULL,        NULL,        NULL,          NULL,		// 68-71
  NULL,        NULL,        NULL,          NULL,		// 72-75
  NULL,        NULL,        NULL,          NULL,		// 76-79
  NULL,        NULL,        NULL,          NULL,		// 80-83
  NULL,        NULL,        NULL,          NULL,		// 84-87
  NULL,        NULL,        NULL,          NULL,		// 88-91
  NULL,        NULL,        NULL,          NULL,		// 92-95
  NULL,        NULL,        NULL,          NULL,		// 96-99
  NULL,        NULL,        NULL,          NULL,		// 100-103
  NULL,        NULL,        NULL,          NULL,		// 104-107
  NULL,        NULL,        NULL,          NULL,		// 108-111
  NULL,        NULL,        NULL,          NULL,		// 112-115
  NULL,        NULL,        NULL,          NULL,		// 116-119
  NULL,        NULL,        NULL,          NULL,		// 120-123
  NULL,        NULL,        NULL,          NULL,		// 124-127
};


static const char *cr_reg_names[128] = {
  "cr.dcr",    "cr.itm",    "cr.iva",    NULL,		// 0-3
  NULL,        NULL,        NULL,        NULL,		// 4-7
  "cr.pta",    NULL,        NULL,        NULL,		// 8-11
  NULL,        NULL,        NULL,        NULL,		// 12-15
  "cr.ipsr",   "cr.isr",    NULL,        "cr.iip",	// 16-19
  "cr.ifa",    "cr.itir",   "cr.iipa",   "cr.ifs",	// 20-23
  "cr.iim",    "cr.iha",    NULL,        NULL,		// 24-27
  NULL,        NULL,        NULL,        NULL,		// 28-31
  NULL,        NULL,        NULL,        NULL,		// 32-35
  NULL,        NULL,        NULL,        NULL,		// 36-39
  NULL,        NULL,        NULL,        NULL,		// 40-43
  NULL,        NULL,        NULL,        NULL,		// 44-47
  NULL,        NULL,        NULL,        NULL,		// 48-51
  NULL,        NULL,        NULL,        NULL,		// 52-55
  NULL,        NULL,        NULL,        NULL,		// 56-59
  NULL,        NULL,        NULL,        NULL,		// 60-63
  "cr.lid",    "cr.ivr",    "cr.tpr",    "cr.eoi",	// 64-67
  "cr.irr0",   "cr.irr1",   "cr.irr2",   "cr.irr3",	// 68-71
  "cr.itv",    "cr.pmv",    "cr.cmcv",   NULL,		// 72-75
  NULL,        NULL,        NULL,        NULL,		// 76-79
  "cr.lrr0",   "cr.lrr1",   NULL,        NULL,		// 80-83
  NULL,        NULL,        NULL,        NULL,		// 84-87
  NULL,        NULL,        NULL,        NULL,		// 88-91
  NULL,        NULL,        NULL,        NULL,		// 92-95
  NULL,        NULL,        NULL,        NULL,		// 96-99
  NULL,        NULL,        NULL,        NULL,		// 100-103
  NULL,        NULL,        NULL,        NULL,		// 104-107
  NULL,        NULL,        NULL,        NULL,		// 108-111
  NULL,        NULL,        NULL,        NULL,		// 112-115
  NULL,        NULL,        NULL,        NULL,		// 116-119
  NULL,        NULL,        NULL,        NULL,		// 120-123
  NULL,        NULL,        NULL,        NULL,		// 124-127
};

static const char *int_reg_names[128] = {
  NULL,  "gp",  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 0-7
  NULL,  NULL,  NULL,  NULL,  "sp",  NULL,  NULL,  NULL,  // 8-15
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 16-23
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 24-31
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 32-39
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 40-47
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 48-55
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 56-63
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 64-71
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 72-79
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 80-87
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 88-95
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 96-103
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 104-111
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 112-119
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  // 120-127
};

static const int addl_regs[] = { 0, 1, 2, 3 };

static const int ec_regs[] = { 66 };
static const int lc_regs[] = { 65 };
static int num_ar_i_regs, ar_i_regs[128];
static int num_ar_m_regs, ar_m_regs[128];

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
    rc_control;

  ISA_Registers_Begin("ia64");

  init_ar_regs();

  rc_integer = ISA_Register_Class_Create("integer", 64, true, false);
  rc_float = ISA_Register_Class_Create("float", 82, true, false);
  rc_predicate = ISA_Register_Class_Create("predicate", 1, false, true);
  rc_branch = ISA_Register_Class_Create("branch", 64, false, false);
  rc_application = ISA_Register_Class_Create("application", 64, false, false);
  rc_control = ISA_Register_Class_Create("control", 64, false, false);

  ISA_Register_Set(rc_integer, 0, 127, "r%d", int_reg_names, All_ISA_Mask());
// 32-127 rotate
// r0 == 0
  ISA_Register_Subclass_Create("addl", rc_integer, 
			       NELEMS(addl_regs), addl_regs, NULL);

  ISA_Register_Set(rc_float, 0, 127, "f%d", NULL, All_ISA_Mask());
// 32-127 rotate
// f0 = 0.0
// f1 = 1.1

  ISA_Register_Set(rc_predicate, 0, 63, "p%d", NULL, All_ISA_Mask());
// 16-63 rotate
// p0 = 1

  ISA_Register_Set(rc_branch, 0, 7, "b%d", NULL, All_ISA_Mask());

  ISA_Register_Set(rc_application, 0, 127, "ar%d", ar_reg_names,
		   All_ISA_Mask());
  ISA_Register_Subclass_Create("ar_i", rc_application, 
			       num_ar_i_regs, ar_i_regs, NULL);
  ISA_Register_Subclass_Create("ar_m", rc_application, 
			       num_ar_m_regs, ar_m_regs, NULL);
  ISA_Register_Subclass_Create("ec", rc_application, 
			       NELEMS(ec_regs), ec_regs, NULL);
  ISA_Register_Subclass_Create("lc", rc_application, 
			       NELEMS(lc_regs), lc_regs, NULL);

  ISA_Register_Set(rc_control, 0, 127, "cr%d", cr_reg_names,
		   All_ISA_Mask());

  ISA_Registers_End();
  return 0;
}
