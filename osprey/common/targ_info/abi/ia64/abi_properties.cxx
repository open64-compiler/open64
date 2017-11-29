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


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/abi/ia64/abi_properties.cxx,v $

#include <stddef.h>
#include "abi_properties_gen.h"
#include "targ_isa_registers.h"

static ABI_PROPERTY
	allocatable,
	callee,
	caller,
	func_arg,
	func_val,
	frame_ptr,
	global_ptr,
	stack_ptr,
	entry_ptr,
	static_link,
	zero,
	ret_addr,
	prev_funcstate,
	loop_count,
	epilog_count,
	true_predicate,
        stacked,
	fzero,
	fone;

static void ia64_abi(void)
{
  static const char *integer_names[128] = {
    NULL,   "gp",   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL,   "sp",   NULL,   NULL,   NULL,
  };

  static const char *branch_names[8] = {
    "rp",   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
  };

  Reg_Names(ISA_REGISTER_CLASS_integer, 0, 127, integer_names);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_integer,
			   2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,            14,  15,
		16,  17,  18,  19,  20,  21,  22,  23, 
		24,  25,  26,  27,  28,  29,  30,  31,
		32,  33,  34,  35,  36,  37,  38,  39,
		40,  41,  42,  43,  44,  45,  46,  47,
		48,  49,  50,  51,  52,  53,  54,  55,
	        56,  57,  58,  59,  60,  61,  62,  63,
	        64,  65,  66,  67,  68,  69,  70,  71,
	        72,  73,  74,  75,  76,  77,  78,  79,
	        80,  81,  82,  83,  84,  85,  86,  87,
	        88,  89,  90,  91,  92,  93,  94,  95,
	        96,  97,  98,  99, 100, 101, 102, 103,
	       104, 105, 106, 107, 108, 109, 110, 111, 
	       112, 113, 114, 115, 116, 117, 118, 119,
	       120, 121, 122, 123, 124, 125, 126, 127,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_integer,
				     4,   5,   6,   7,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_integer,
		 2,   3,   8,   9,  10,  11,  14,  15, 
		16,  17,  18,  19,  20,  21,  22,  23,
		24,  25,  26,  27,  28,  29,  30,  31,
	       -1);
  Reg_Property(stacked, ISA_REGISTER_CLASS_integer,
		32,  33,  34,  35,  36,  37,  38,  39,
		40,  41,  42,  43,  44,  45,  46,  47,
		48,  49,  50,  51,  52,  53,  54,  55,
	        56,  57,  58,  59,  60,  61,  62,  63,
	        64,  65,  66,  67,  68,  69,  70,  71,
	        72,  73,  74,  75,  76,  77,  78,  79,
	        80,  81,  82,  83,  84,  85,  86,  87,
	        88,  89,  90,  91,  92,  93,  94,  95,
	        96,  97,  98,  99, 100, 101, 102, 103,
	       104, 105, 106, 107, 108, 109, 110, 111, 
	       112, 113, 114, 115, 116, 117, 118, 119,
	       120, 121, 122, 123, 124, 125, 126, 127,
	       -1);
  Reg_Property(func_arg, ISA_REGISTER_CLASS_integer,
		32,  33,  34,  35,  36,  37,  38,  39,
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_integer,
		 8,   9,  10,  11,
	       -1);
  Reg_Property(static_link, ISA_REGISTER_CLASS_integer, 
		14,
	       -1);
  Reg_Property(global_ptr, ISA_REGISTER_CLASS_integer, 
		1,
	       -1);
  Reg_Property(frame_ptr, ISA_REGISTER_CLASS_integer, 
		7,
	       -1);
  Reg_Property(stack_ptr, ISA_REGISTER_CLASS_integer, 
		12,
	       -1);
  Reg_Property(zero, ISA_REGISTER_CLASS_integer, 
		0,
	       -1);

  // float register class:

  Reg_Property(allocatable, ISA_REGISTER_CLASS_float,
			   2,   3,   4,   5,   6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
		16,  17,  18,  19,  20,  21,  22,  23,
		24,  25,  26,  27,  28,  29,  30,  31,
		32,  33,  34,  35,  36,  37,  38,  39,
		40,  41,  42,  43,  44,  45,  46,  47,
		48,  49,  50,  51,  52,  53,  54,  55,
		56,  57,  58,  59,  60,  61,  62,  63,
		64,  65,  66,  67,  68,  69,  70,  71,
		72,  73,  74,  75,  76,  77,  78,  79,
		80,  81,  82,  83,  84,  85,  86,  87,
		88,  89,  90,  91,  92,  93,  94,  95,
		96,  97,  98,  99, 100, 101, 102, 103,
	       104, 105, 106, 107, 108, 109, 110, 111,
	       112, 113, 114, 115, 116, 117, 118, 119, 
	       120, 121, 122, 123, 124, 125, 126, 127,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_float,
		 2,   3,   4,   5,  16,  17,  18,  19,
		20,  21,  22,  23,  24,  25,  26,  27,
		28,  29,  30,  31,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_float,
		 6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
		32,  33,  34,  35,  36,  37,  38,  39,
		40,  41,  42,  43,  44,  45,  46,  47,
		48,  49,  50,  51,  52,  53,  54,  55,
		56,  57,  58,  59,  60,  61,  62,  63,
		64,  65,  66,  67,  68,  69,  70,  71,
		72,  73,  74,  75,  76,  77,  78,  79,
		80,  81,  82,  83,  84,  85,  86,  87,
		88,  89,  90,  91,  92,  93,  94,  95,
		96,  97,  98,  99, 100, 101, 102, 103,
	       104, 105, 106, 107, 108, 109, 110, 111,
	       112, 113, 114, 115, 116, 117, 118, 119, 
	       120, 121, 122, 123, 124, 125, 126, 127,
	       -1);
  Reg_Property(func_arg, ISA_REGISTER_CLASS_float,
		 8,   9,  10,  11,  12,  13,  14,  15,
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_float,
		 8,   9,  10,  11,  12,  13,  14,  15,
	       -1);
  Reg_Property(fzero, ISA_REGISTER_CLASS_float, 
		 0,
	       -1);
  Reg_Property(fone, ISA_REGISTER_CLASS_float, 
		 1,
	       -1);

  // predicate register class:

  Reg_Property(allocatable, ISA_REGISTER_CLASS_predicate,
		      1,   2,   3,   4,   5,   6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
		16,  17,  18,  19,  20,  21,  22,  23,
		24,  25,  26,  27,  28,  29,  30,  31,
		32,  33,  34,  35,  36,  37,  38,  39,
		40,  41,  42,  43,  44,  45,  46,  47,
		48,  49,  50,  51,  52,  53,  54,  55,
		56,  57,  58,  59,  60,  61,  62,  63,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_predicate,
		 1,   2,   3,   4,   5,
		16,  17,  18,  19,  20,  21,  22,  23,
		24,  25,  26,  27,  28,  29,  30,  31,
		32,  33,  34,  35,  36,  37,  38,  39,
		40,  41,  42,  43,  44,  45,  46,  47,
		48,  49,  50,  51,  52,  53,  54,  55,
		56,  57,  58,  59,  60,  61,  62,  63,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_predicate,
					      6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
	       -1);
  Reg_Property(true_predicate, ISA_REGISTER_CLASS_predicate, 
	       0,
	       -1);

  // branch register class:

  Reg_Names(ISA_REGISTER_CLASS_branch, 0, 7, branch_names);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_branch,
		 0,   1,   2,   3,   4,   5,   6,   7,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_branch,
		      1,   2,   3,   4,   5,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_branch,
		 0,                            6,   7,
	       -1);
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_branch, 
	       0,
	       -1);

  // application register class:

  Reg_Property(prev_funcstate, ISA_REGISTER_CLASS_application, 
		64,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_application, 
		32,  64,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_application, 
		36,  65,  66,
	       -1);
  Reg_Property(loop_count, ISA_REGISTER_CLASS_application, 
		65,
	       -1);
  Reg_Property(epilog_count, ISA_REGISTER_CLASS_application, 
		66,
	       -1);

  // control register class:

  /* nothing! */
}

main()
{
  ABI_Properties_Begin("ia64");

  allocatable = Create_Reg_Property("allocatable");
  callee = Create_Reg_Property("callee");
  caller = Create_Reg_Property("caller");
  stacked = Create_Reg_Property("stacked");
  func_arg = Create_Reg_Property("func_arg");
  func_val = Create_Reg_Property("func_val");
  frame_ptr = Create_Reg_Property("frame_ptr");
  global_ptr = Create_Reg_Property("global_ptr");
  stack_ptr = Create_Reg_Property("stack_ptr");
  entry_ptr = Create_Reg_Property("entry_ptr");
  static_link = Create_Reg_Property("static_link");
  zero = Create_Reg_Property("zero");
  ret_addr = Create_Reg_Property("ret_addr");
  prev_funcstate = Create_Reg_Property("prev_funcstate");
  loop_count = Create_Reg_Property("loop_count");
  epilog_count = Create_Reg_Property("epilog_count");
  true_predicate = Create_Reg_Property("true_predicate");
  fzero = Create_Reg_Property("fzero");
  fone = Create_Reg_Property("fone");

  ///////////////////////////////////////
  Begin_ABI("i32");
  ia64_abi();

  ///////////////////////////////////////
  Begin_ABI("i64");
  ia64_abi();

  ABI_Properties_End();
}
