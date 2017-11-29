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


//  
//  Generate ISA bundle information
///////////////////////////////////////

//  $Revision: 1.38 $
//  $Date: 2001/03/10 01:16:06 $
//  $Author: mtibuild $
//  $Source: /osprey.src/osprey1.0/common/targ_info/isa/ia64/RCS/isa_bundle.cxx,v $

#include <stddef.h>
#include "topcode.h"
#include "isa_bundle_gen.h"

main()
{
  ISA_Bundle_Begin("nvisa", 32);

  // there are no bundles so just create dummy one
  ISA_Bundle_Pack_Create(ISA_Bundle_Pack_Little_Endian);
  Pack_Template(0, 0, 0);

  ISA_Bundle_End();
  return 0;
}
