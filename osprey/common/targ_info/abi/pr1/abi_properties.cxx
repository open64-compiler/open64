/*
 * Copyright 2004, PacRidge, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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
//  Generate ABI information
///////////////////////////////////////


//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:18:15 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/abi/pr1/abi_properties.cxx,v $

#include <stddef.h>
#include "abi_properties_gen.h"
#include "targ_isa_registers.h"

static ABI_PROPERTY
  allocatable,
  callee,
  caller,
  func_arg,
  func_val,
  stack_ptr,
  frame_ptr,
  static_link,
  zloop,
  global_ptr,
  entry_ptr,
  ret_addr,
    loop_count;

static void pr1_abi(void)
{
  Reg_Property(allocatable, ISA_REGISTER_CLASS_integer,
		 0,   1,   2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_integer,
                12,  13,  14,  15,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_integer,
	         0,   1,   2,   3,   4,   5,   6,   7,
	         8,   9,  10,  11,
	       -1);
  Reg_Property(func_arg, ISA_REGISTER_CLASS_integer,
		 0,  1,  2,  3,  4,  5,
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_integer,
		 6,  7, 
	       -1);
  Reg_Property(loop_count, ISA_REGISTER_CLASS_special,
		 5,   
	       -1);
  Reg_Property(static_link, ISA_REGISTER_CLASS_special,
		 2,   
	       -1);
  Reg_Property(global_ptr, ISA_REGISTER_CLASS_special, 
		 3,
	       -1);
  Reg_Property(stack_ptr, ISA_REGISTER_CLASS_special, 
		 2,
	       -1);
  Reg_Property(frame_ptr, ISA_REGISTER_CLASS_special, 
		 2,
	       -1);
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_special, 
		 4,
	       -1);


}

main()
{
  ABI_Properties_Begin("pr1");

  allocatable = Create_Reg_Property("allocatable");
  callee = Create_Reg_Property("callee");
  caller = Create_Reg_Property("caller");
  func_arg = Create_Reg_Property("func_arg");
  func_val = Create_Reg_Property("func_val");
  stack_ptr = Create_Reg_Property("stack_ptr");
  frame_ptr = Create_Reg_Property("frame_ptr");
  static_link = Create_Reg_Property("static_link");
  global_ptr = Create_Reg_Property("global_ptr");
  entry_ptr = Create_Reg_Property("entry_ptr");
  ret_addr = Create_Reg_Property("ret_addr");
  loop_count = Create_Reg_Property("loop_count");
  
  Begin_ABI("pr1");
  pr1_abi();
  
  ABI_Properties_End();
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
