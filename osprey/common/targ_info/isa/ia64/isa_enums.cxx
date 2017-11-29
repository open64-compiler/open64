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
// Generate a list of enumeration classes and values for the ISA.
/////////////////////////////////////////////////////////

#include <stddef.h>
#include "isa_enums_gen.h"

main ()
{
  ISA_Enums_Begin();

  ISA_Create_Enum_Class ("mwh",
      ".sptk",		0,
      ".dptk",		2,
      NULL,		1);	// default value

  ISA_Create_Enum_Class ("ldhint",
      ".nt1",		1,
      ".nta",		3,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("sthint",
      ".nta",		3,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("lfhint",
      ".nt1",		1,
      ".nt2",		2,
      ".nta",		3,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("ph", 
      ".few",		0,
      ".many",		1,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("bwh", 
      ".sptk",		0,
      ".spnt",		1,
      ".dptk",		2,
      ".dpnt",		3,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("dh", 
      ".clr",		1,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("ipwh", 
      ".sptk",		0,
      ".loop",		1,
      ".dptk",		2,
      ".exit",		3,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("indwh", 
      ".sptk",		0,
      ".dptk",		2,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("ih", 
      ".imp",		1,
      NULL,		0);		// default value

  ISA_Create_Enum_Class ("aclr",
      ".nc",		0,
      ".clr",		1,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("sem", 
      ".acq",		0,
      ".rel",		1,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("ldtype",
      ".s",		1,
      ".a",		2,
      ".sa",		3,
      ".bias",		4,
      ".acq",		5,
      ".c.clr",		8,
      ".c.nc",		9,
      ".c.clr.acq",	10,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("fldtype",
      ".s",		1,
      ".a",		2,
      ".sa",		3,
      ".c.clr",		8,
      ".c.nc",		9,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("sttype",
      ".rel",		1,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("mbtype4",
      "@brdcst",	0,
      "@mix",		8,
      "@shuf",		9,
      "@alt",		0xa,
      "@rev",		0xb,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("sf",
      ".s0",		0,
      ".s1",		1,
      ".s2",		2,
      ".s3",		3,
      NULL,		UNDEFINED);	// default value

  ISA_Enums_End();
}
