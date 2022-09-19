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
//  Generate ISA bundle information
///////////////////////////////////////

//  $Revision$
//  $Date$
//  $Author$
//  $Source$

#include <stddef.h>
#include "topcode.h"
#include "isa_bundle_gen.h"

main()
{
  ISA_EXEC_UNIT_TYPE
    Fetch_Unit;  // Instruction fetch type

  ISA_Bundle_Begin("x8664", 32);

  ISA_Bundle_Pack_Create(ISA_Bundle_Pack_Little_Endian);
  Pack_Slot(0, 0, 0, 24);

  /* ===== Specification for Fetch_Unit Type ===== */
  Fetch_Unit = ISA_Exec_Unit_Type_Create("Fetch_Unit", NULL);
  Instruction_Exec_Unit_Group(Fetch_Unit,
			      TOP_UNDEFINED);

  ISA_Bundle_Type_Create("i", ".i", 1);
  Slot (0, Fetch_Unit);

  ISA_Bundle_End();
  return 0;
}
