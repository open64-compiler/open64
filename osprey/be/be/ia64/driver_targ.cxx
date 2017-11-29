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


#include "defs.h"
#include "errors.h"
#include "config.h"
#include "config_targ.h"
#include "ti_init.h"

void Initialize_Targ_Info(void)
{
  ABI_PROPERTIES_ABI abi;
  ISA_SUBSET isa;
  PROCESSOR proc;
  char version[2]="";

  switch (Target_ABI) {
  case ABI_I32:
    abi = ABI_PROPERTIES_ABI_i32;
    break;
  case ABI_I64:
    abi = ABI_PROPERTIES_ABI_i64;
    break;
  default:
    FmtAssert(FALSE, ("targinfo doesn't handle abi: %d\n", Target_ABI));
    /*NOTREACHED*/
  }

  switch (Target_ISA) {
  case TARGET_ISA_I1:
    isa = ISA_SUBSET_intel1;
    break;
  default:
    FmtAssert(FALSE, ("targinfo doesn't handle isa: %s\n", Isa_Name(Target_ISA)));
    /*NOTREACHED*/
  }

  switch (Target) {
  case TARGET_ITANIUM:
    proc = PROCESSOR_itanium;
    break;

  case TARGET_ITANIUM2:
    proc = PROCESSOR_itanium;
    version[0] = '2';
    version[1] = '\0';
    break;

  default:
    FmtAssert(FALSE, ("targinfo doesn't handle target: %s\n", Targ_Name(Target)));
    /*NOTREACHED*/
  }

  TI_Initialize(abi, isa, proc, Targ_Path, version);
}
