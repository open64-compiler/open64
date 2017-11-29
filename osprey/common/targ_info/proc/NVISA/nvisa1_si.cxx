/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
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


//  NVISA processor scheduling information
/////////////////////////////////////
//  
//  Description:
//
//  Generate a scheduling description of a target processor
//  via the si_gen interface.
//
/////////////////////////////////////


//  $Revision: 1.35 $
//  $Date: 2001/03/10 01:17:30 $
//  $Author: mtibuild $
//  $Source: /osprey.src/osprey1.0/common/targ_info/proc/ia64/RCS/itanium_si.cxx,v $


#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_issue;

void Generate_NVISA1 (void)
{
  Machine("nvisa1", ISA_SUBSET_nvisa1);

  res_issue = RESOURCE_Create("issue", 6);

  ////////////////////////////////////////////////////
  //
  // all insts have dummy latency of 1
  // Don't need to list all insts to build?
  // Won't be really using this schedule.
  //
  ////////////////////////////////////////////////////
  Instruction_Group("dummy",
		TOP_ld_s8, TOP_ld_s16, TOP_ld_s32, TOP_ld_s64,
		TOP_ld_u8, TOP_ld_u16, TOP_ld_u32, TOP_ld_u64,
		TOP_ld_f16, TOP_ld_f32, TOP_ld_f64,
		TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);

  Machine_Done();
}

