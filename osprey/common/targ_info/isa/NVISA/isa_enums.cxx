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
// Generate a list of enumeration classes and values for the ISA.
/////////////////////////////////////////////////////////

#include <stddef.h>
#include "isa_enums_gen.h"

main ()
{
  ISA_Enums_Begin();

  ISA_Create_Enum_Class ("cmp",
	"eq",		1,
	"ne",		2,
	"lt",		3,
	"le",		4,
	"gt",		5,
	"ge",		6,
	"lo",		7,
	"ls",		8,
	"hi",		9,
	"hs",		10,
	"equ",		11,
	"neu",		12,
	"ltu",		13,
	"leu",		14,
	"gtu",		15,
	"geu",		16,
	"nan",		17,
	"num",		18,
	NULL,		0);	// default value

  ISA_Create_Enum_Class ("boolop",
	"and",		1,
	"or",		2,
	"xor",		3,
	NULL,		0);	// default value

  ISA_Create_Enum_Class ("sat",
	"sat",		1,
	NULL,		0);	// default value

  ISA_Create_Enum_Class ("iround",
	"rni",		1,
	"rmi",		2,
	"rpi",		3,
	"rzi",		4,
	NULL,		0);	// default value

  ISA_Create_Enum_Class ("fround",
	"rn",		1,
	"rm",		2,
	"rp",		3,
	"rz",		4,
	NULL,		0);	// default value

  ISA_Create_Enum_Class ("space",
	"global",	1,
	"shared",	2,
	"param",	3,
	"const",	4,
	"local",	5,
	NULL,		0);	// default value

  ISA_Create_Enum_Class ("qualifier",
	".volatile",	1,
	NULL,		0);	// default value
  ISA_Enums_End();
}
